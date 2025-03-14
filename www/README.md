# Static Files Directory

This directory contains static files that are automatically served by the Shiny app, including:

- `index.html`: A landing page that is shown if users access the app directly through a web browser
- Images, CSS, and other static assets used by the application

## Adding Custom Assets

You can add additional static files to this directory:

1. Images: Place image files in this directory to use them in the app
2. Custom CSS: Add custom styling by creating a CSS file here
3. JavaScript: Add custom client-side functionality with JS files

## Usage in Shiny

To reference files in this directory from your Shiny app:

```R
# Reference an image in the UI
tags$img(src = "logo.png", height = "50px")

# Include a custom CSS file
tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))

# Include a JavaScript file
tags$head(tags$script(src = "custom.js"))
```

## Notes

- Files in this directory are accessible at the root URL of the Shiny app
- No special path is needed when referencing these files in your app
- The directory is automatically created and managed by Shiny
