maak_carousel <- function(image_dir, carousel_id, caption_tekst) {
  # Haal alle afbeeldingen op
  images <- list.files(image_dir, pattern = "\\.(jpg|jpeg|png|gif)$", ignore.case = TRUE)
  images <- sort(images)
  
  if (length(images) > 0) {
    html_output <- c()
    
    
    # Start de Bootstrap Carousel structuur met uniek ID
    html_output <- c(html_output, sprintf('<div id="carousel-%s" class="carousel slide" data-bs-ride="carousel">', carousel_id))
    html_output <- c(html_output, '  <div class="carousel-inner">')
    
    # Loop door alle figuren heen
    for (i in seq_along(images)) {
      img_name <- images[i]
      img_path <- file.path(image_dir, img_name)
      schoon_label <- tools::file_path_sans_ext(img_name)
      active_class <- if (i == 1) "active" else ""
      
      html_output <- c(html_output, sprintf('    <div class="carousel-item %s">', active_class))
      html_output <- c(html_output, sprintf('      <img src="%s" class="d-block w-100" alt="%s">', img_path, img_name))
      html_output <- c(html_output, '      <div class="carousel-caption d-none d-md-block">')
      html_output <- c(html_output, sprintf('        <h5>%s</h5>', schoon_label))
      html_output <- c(html_output, '      </div>')
      html_output <- c(html_output, '    </div>')
    }
    
    # Sluit de carousel en voeg knoppen toe gekoppeld aan het unieke ID
    html_output <- c(html_output, '  </div>')
    html_output <- c(html_output, sprintf('  <button class="carousel-control-prev" type="button" data-bs-target="#carousel-%s" data-bs-slide="prev">', carousel_id))
    html_output <- c(html_output, '    <span class="carousel-control-prev-icon" aria-hidden="true"></span>')
    html_output <- c(html_output, '  </button>')
    html_output <- c(html_output, sprintf('  <button class="carousel-control-next" type="button" data-bs-target="#carousel-%s" data-bs-slide="next">', carousel_id))
    html_output <- c(html_output, '    <span class="carousel-control-next-icon" aria-hidden="true"></span>')
    html_output <- c(html_output, '  </button>')
    html_output <- c(html_output, '</div>')
    
    # Voeg de algemene caption toe en sluit de Quarto div
    html_output <- c(html_output, '')
    html_output <- c(html_output, caption_tekst)
    ##html_output <- c(html_output, ':::')
    
    # Stuur het resultaat door naar knitr als pure HTML
    volledige_html <- paste(html_output, collapse = "\n")
    cat(knitr::raw_html(volledige_html))
    
  } else {
    cat(sprintf('<p style="color:red;">Waarschuwing: Geen afbeeldingen gevonden in %s.</p>', image_dir))
  }
}