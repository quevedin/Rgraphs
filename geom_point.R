GeomPoint_old <- ggplot2:::GeomPoint

# Define the new version
library(proto)
GeomPoint_new <- proto(ggplot2:::Geom, {
  objname <- "point"
  
  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm,
                           c("x", "y", "size", "shape","width"), name = "geom_point")
    if (empty(data)) return(zeroGrob())
    
    with(coord_transform(coordinates, data, scales),
         ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
                                        gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), 
                                                fontsize = size * .pt,
                                                lex = width)))
    )
  }
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    
    with(data,
         pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape,
                    gp=gpar(
                      col=alpha(colour, alpha),
                      fill=alpha(fill, alpha),
                      fontsize = size * .pt,
                      lex = width)
         )
    )
  }
  
  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(shape=16, colour="black", size=2, fill = NA, alpha = NA, width = 0.6)
  
})

# Make the new function run in the same environment
environment(GeomPoint_new) <- environment(ggplot2:::GeomPoint)

# Replace ggplot2:::GeomPoint with GeomPoint_new
assignInNamespace("GeomPoint", GeomPoint_new, ns="ggplot2")
