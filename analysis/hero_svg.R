# Animated header image for the README: the tag co-occurrence network fades to
# its disparity-filter backbone. Pure SVG + CSS (no JavaScript), so it animates
# when GitHub shows it as an image. The un-animated state is the backbone with
# labels, which is what readers who prefer reduced motion see.

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

hero_colours <- list(
  light = c(edge = "#8c959f", backbone = "#24292f", text = "#1f2328", halo = "#ffffff", muted = "#59636e"),
  dark  = c(edge = "#6e7681", backbone = "#e6edf3", text = "#e6edf3", halo = "#0d1117", muted = "#9198a1")
)

# Greedy label placement. In the given (priority) order, try right of, left of,
# above and below each node, and keep the spot that overlaps least with labels
# already placed, the other labelled nodes and any `avoid` boxes (x0, x1, y0,
# y1); spots outside [xmin, xmax] x [ymin, ymax] are heavily penalised. Text
# widths are estimated for 15px bold sans.
place_labels <- function(x, y, r, text, xmin, xmax, ymin, ymax, avoid = NULL,
                         char_w = 8.6, h = 16) {
  w <- char_w * nchar(text)
  if (is.null(avoid)) avoid <- matrix(numeric(0), ncol = 4)
  overlap <- function(b, m) {
    sum(pmax(0, pmin(b[2], m[, 2]) - pmax(b[1], m[, 1])) *
          pmax(0, pmin(b[4], m[, 4]) - pmax(b[3], m[, 3])))
  }
  node_box <- cbind(x - r, x + r, y - r, y + r)
  placed   <- matrix(numeric(0), ncol = 4)
  out <- data.frame(tx = numeric(0), ty = numeric(0), anchor = character(0),
                    x0 = numeric(0), x1 = numeric(0), y0 = numeric(0), y1 = numeric(0))
  for (i in seq_along(text)) {
    gap <- r[i] + 4
    cand <- list(
      list(tx = x[i] + gap, ty = y[i] + 5, anchor = "start",
           box = c(x[i] + gap, x[i] + gap + w[i], y[i] - h / 2, y[i] + h / 2)),
      list(tx = x[i] - gap, ty = y[i] + 5, anchor = "end",
           box = c(x[i] - gap - w[i], x[i] - gap, y[i] - h / 2, y[i] + h / 2)),
      list(tx = x[i], ty = y[i] - gap - 2, anchor = "middle",
           box = c(x[i] - w[i] / 2, x[i] + w[i] / 2, y[i] - gap - h, y[i] - gap)),
      list(tx = x[i], ty = y[i] + gap + 12, anchor = "middle",
           box = c(x[i] - w[i] / 2, x[i] + w[i] / 2, y[i] + gap, y[i] + gap + h)))
    score <- vapply(cand, function(cd) {
      b <- cd$box
      outside <- max(0, xmin - b[1]) + max(0, b[2] - xmax) + max(0, ymin - b[3]) + max(0, b[4] - ymax)
      1e6 * outside + 3 * overlap(b, placed) + overlap(b, node_box[-i, , drop = FALSE]) +
        2 * overlap(b, avoid)
    }, numeric(1))
    best   <- cand[[which.min(score)]]
    placed <- rbind(placed, best$box)
    out[i, ] <- list(best$tx, best$ty, best$anchor, best$box[1], best$box[2], best$box[3], best$box[4])
  }
  out
}

# nodes: label, x, y, degree, community, in_backbone, show_label, and bx, by
#        (the node's position in the backbone layout; NA outside the backbone)
# edges: from, to (row indices into nodes), weight, backbone
write_hero_svg <- function(path, nodes, edges, palette, mode = c("light", "dark"),
                           caption_full, caption_backbone,
                           width = 1200, height = 640, cycle = 12) {
  mode <- match.arg(mode)
  col  <- hero_colours[[mode]]
  num  <- function(v, digits = 1) formatC(v, format = "f", digits = digits)

  # Both layouts fill the full height in the middle of the canvas (caption on
  # the left, legend on the right), undistorted and with y pointing up as in
  # the ggplot figures
  left <- 300; right <- width - 300; top <- 25; bottom <- height - 25
  fit <- function(x, y) {
    s <- min((right - left) / diff(range(x)), (bottom - top) / diff(range(y)))
    list(x = (x - mean(range(x))) * s + (left + right) / 2,
         y = (top + bottom) / 2 - (y - mean(range(y))) * s)
  }
  full <- fit(nodes$x, nodes$y)
  px <- full$x
  py <- full$y
  # Backbone nodes glide from the full layout into the backbone's own layout
  moving <- nodes$in_backbone
  bbxy <- fit(nodes$bx[moving], nodes$by[moving])
  qx <- px
  qy <- py
  qx[moving] <- bbxy$x
  qy[moving] <- bbxy$y

  line_tags <- function(e, w, x, y) {
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke-width="%s"/>',
            num(x[e$from]), num(y[e$from]), num(x[e$to]), num(y[e$to]), num(w, 2))
  }
  bb <- edges[edges$backbone, ]
  r  <- 2.2 + 1.1 * sqrt(nodes$degree)
  shift <- ifelse(moving, sprintf(' style="--dx:%spx;--dy:%spx"', num(qx - px), num(qy - py)), "")
  circles <- sprintf('<circle class="%s" cx="%s" cy="%s" r="%s" fill="%s"%s><title>%s</title></circle>',
                     ifelse(moving, "nb", "n"), num(px), num(py), num(r),
                     palette[as.character(nodes$community)], shift, xml_escape(nodes$label))

  # Labels sit at the backbone positions, placed greedily (most connected
  # first) clear of each other, the caption block and the other backbone nodes
  lab    <- which(nodes$show_label)
  lab    <- lab[order(-nodes$degree[lab])]
  others <- which(moving & !nodes$show_label)
  avoid  <- rbind(c(30, 345, 40, 112),
                  cbind(qx[others] - r[others], qx[others] + r[others],
                        qy[others] - r[others], qy[others] + r[others]))
  pl     <- place_labels(qx[lab], qy[lab], r[lab], nodes$label[lab],
                         xmin = 250, xmax = width - 270, ymin = 8, ymax = height - 8, avoid = avoid)
  labels <- sprintf('<text x="%s" y="%s" text-anchor="%s">%s</text>',
                    num(pl$tx), num(pl$ty), pl$anchor, xml_escape(nodes$label[lab]))

  # Legend: a vertical list, vertically centred, at the right edge
  ly     <- height / 2 + 28 * (seq_along(palette) - (length(palette) + 1) / 2)
  lx     <- width - 255
  legend <- sprintf('<circle cx="%s" cy="%s" r="7" fill="%s"/><text class="lg" x="%s" y="%s">%s</text>',
                    num(lx), num(ly), palette, num(lx + 16), num(ly + 5), xml_escape(names(palette)))

  # Captions: one line per element, top left; the two captions cross-fade
  caption <- function(lines, class) {
    dy <- c("0", rep("1.35em", length(lines) - 1))
    sprintf('<text class="cap %s" x="40" y="70">%s</text>', class,
            paste(sprintf('<tspan x="40" dy="%s">%s</tspan>', dy, xml_escape(lines)), collapse = ""))
  }

  css <- sprintf(paste(
    'svg { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "Helvetica Neue", Helvetica, Arial, sans-serif; }',
    '.edges line { stroke: %1$s; stroke-opacity: .03; animation: edges %2$ss ease-in-out infinite; }',
    '.backbone line { stroke: %3$s; stroke-opacity: .8; stroke-linecap: round; animation: backbone %2$ss ease-in-out infinite; }',
    'circle.n, circle.nb { stroke: %4$s; stroke-width: 1; }',
    'circle.n { opacity: .2; animation: fade %2$ss ease-in-out infinite; }',
    'circle.nb { transform: translate(var(--dx), var(--dy)); animation: move %2$ss ease-in-out infinite; }',
    '.labels text { font-size: 15px; font-weight: 600; fill: %5$s; paint-order: stroke; stroke: %4$s; stroke-width: 4px; stroke-linejoin: round; animation: appear %2$ss ease-in-out infinite; }',
    '.lg { font-size: 15px; fill: %6$s; }',
    '.cap { font-size: 20px; font-weight: 600; fill: %5$s; }',
    '.cap-full { opacity: 0; animation: capfull %2$ss ease-in-out infinite; }',
    '.cap-bb { animation: appear %2$ss ease-in-out infinite; }',
    '@keyframes edges { 0%%, 20%% { stroke-opacity: .32 } 40%%, 85%% { stroke-opacity: .03 } 100%% { stroke-opacity: .32 } }',
    '@keyframes backbone { 0%%, 28%% { stroke-opacity: 0 } 44%%, 85%% { stroke-opacity: .8 } 100%% { stroke-opacity: 0 } }',
    '@keyframes move { 0%%, 20%% { transform: translate(0px, 0px) } 40%%, 85%% { transform: translate(var(--dx), var(--dy)) } 100%% { transform: translate(0px, 0px) } }',
    '@keyframes fade { 0%%, 20%% { opacity: 1 } 40%%, 85%% { opacity: .2 } 100%% { opacity: 1 } }',
    '@keyframes appear { 0%%, 30%% { opacity: 0 } 46%%, 83%% { opacity: 1 } 100%% { opacity: 0 } }',
    '@keyframes capfull { 0%%, 18%% { opacity: 1 } 30%%, 88%% { opacity: 0 } 100%% { opacity: 1 } }',
    '@media (prefers-reduced-motion: reduce) { * { animation: none !important; } }',
    sep = "\n"),
    col[["edge"]], cycle, col[["backbone"]], col[["halo"]], col[["text"]], col[["muted"]])

  svg <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d" ',
                   'role="img" aria-labelledby="hero-title hero-desc">'), width, height, width, height),
    '<title id="hero-title">Steam tag co-occurrence network and its backbone</title>',
    sprintf('<desc id="hero-desc">%s, fading to the %s.</desc>',
            xml_escape(paste(caption_full, collapse = ", ")),
            xml_escape(paste(caption_backbone, collapse = ", "))),
    '<style>', css, '</style>',
    '<g class="edges">', line_tags(edges, 0.35 + 0.22 * sqrt(edges$weight), px, py), '</g>',
    '<g class="backbone">', line_tags(bb, 0.9 + 0.5 * sqrt(bb$weight), qx, qy), '</g>',
    '<g class="nodes">', circles, '</g>',
    '<g class="labels">', labels, '</g>',
    '<g class="legend">', legend, '</g>',
    caption(caption_full, "cap-full"),
    caption(caption_backbone, "cap-bb"),
    '</svg>')
  # Write UTF-8 bytes as-is: an encoding-converting connection would first
  # translate to the native encoding and truncate non-ASCII text under a C locale
  con <- file(path, open = "wb")
  writeLines(enc2utf8(svg), con, useBytes = TRUE)
  close(con)
  invisible(path)
}
