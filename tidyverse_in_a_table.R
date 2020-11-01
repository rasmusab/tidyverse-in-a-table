library(DT)
library(glue)
library(tidyverse)

package_names <- tidyverse:::core

tidyverse_functions <- map_dfr(package_names, function(package_name) {
  tibble(
    Package = package_name,
    Function = as.character(lsf.str(glue("package:{Package}"))),
    Arguments = map_chr(Function, function(function_name) {
      capture.output(str(get(function_name))) %>% 
        str_squish() %>% 
        str_c(collapse = " ") %>% 
        str_remove("^function ")
    })
  ) 
})

# plucking out non-exported Rd helper functions from the tools package
Rd_get_metadata <- tools:::.Rd_get_metadata
Rd_contents <- tools:::Rd_contents
Rd_get_example_code <- tools:::.Rd_get_example_code
Rd_get_section <- tools:::.Rd_get_section
Rd_get_text <- tools:::.Rd_get_text

# Extracts the text of the named section from the rd_doc
Rd_get_section_text <- function(rd_doc, section) {
  Rd_get_section(rd_doc, section) %>% 
    Rd_get_text() %>% 
    discard(~ .x == "")
}

rd_info <- map_dfr(package_names, function(package_name) {
  # A list with the parsed package documentation
  rd_list <- tools::Rd_db(package_name)
  rd_list %>% 
    # Turn the documentation contents into a data frame, one doc page per row.
    Rd_contents() %>% 
    as_tibble() %>% 
    # Remove all documentation of datasets and "internal" functions. We need to use 
    # map_lgl here as Keywords is a list of character vectors.
    filter(map_lgl(Keywords, ~ length(.x) == 0 || ! .x %in% c("datasets", "internal"))) %>% 
    select(File, Name, Title, Aliases, Keywords) %>% 
    mutate(Package = package_name) %>% 
    # For each row/doc page we're going to extract some information
    rowwise() %>% 
    mutate(
      rd_doc = list(rd_list[[File]]),
      # The function examples. We're using the paste0("", x) trick as x might be a 
      # 0-length vector, but we still want to get back an empty (1-length) character vector.
      Examples = str_trim(paste0("", Rd_get_example_code(rd_doc))),
      Description = paste(Rd_get_section_text(rd_doc, "description"), collapse = " "),
      # A single doc page can document many function we here make a list of all functions
      # documented by the doc page.
      names_and_aliases = list(unique(c(Name, Aliases)))) %>% 
    # A page can document many functions and this  will get us one row per function instead of
    # one row per doc page. All row values, except those in names_and_aliases, will be duplicated.
    unnest(names_and_aliases)
})

tidyverse_functions_info <- inner_join(
  tidyverse_functions, rd_info,
  by = c("Package" = "Package", "Function" = "names_and_aliases"))

tidyverse_functions_info %>% 
  select(Package, Function, Arguments, Title, Description) %>% 
  # Show a datatable with five visible rows
  datatable(options = list(pageLength = 5))

formatted_functions_info <- tidyverse_functions_info %>%  
  mutate(
    # The package and function name links to the tidyverse.org documentation 
    Function = glue("<a href='https://{Package}.tidyverse.org/reference/{Name}.html'>{Function}</a>"),
    Package = glue("<a href='https://{Package}.tidyverse.org/'>{Package}</a>"),
    # Let's replace all space in the arguments with non-breaking space (&nbsp;)
    # except after a comma, so that text only wraps between arguments.
    Arguments = str_replace_all(Arguments, "(?<!,) " , "&nbsp;"),
    # Join the Title and Description, and format the examples 
    Description = glue("<b>{Title}</b><br>{Description}"),
    Examples = glue("<b>Examples</b><pre><code>{Examples}</pre></code>"),
    # A mystery column consisting only of pluses (&oplus;), read on for explanation!
    " " = '&oplus;') %>% 
  select(Package, Function, Arguments, Description,  Examples, ` `)

datatable_callback <- JS("
  var format = function(d) {
    return '<div style=\"padding: .5em;\">' +
           '<p>' + d[4] + '</p>' +
           '<p>' + d[5] + '</p>' + 
           '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
)

tidyverse_in_a_table <- datatable(
  formatted_functions_info, 
  # Render HTML in the table
  escape = FALSE, 
  # Add search boxes for each column at the "top" of the table
  filter = "top",  
  # Register the javascript code we wrote above as a callback
  callback = datatable_callback,
  # To shorten the descriptions we're going to use the datatable ellipsis plugin
  # which adds ... when the text in a cell is too long.
  plugins = "ellipsis",
  options =  list(
    # Show 5 rows by default
    pageLength = 5,
    # But it will be possible to show up to a 100 rows!
    lengthMenu = c(5, 10, 20, 100),
    # Some column specific settings
    columnDefs = list(
      # column 0 (row numbers) and 6 (Examples) are hidden
      list(visible = FALSE, targets = c(0, 5)),
      # The special column with (+) gets the details-control class so that it
      # triggers the callback code
      list(orderable = FALSE, className = 'details-control', targets = 6),
      # Adds an ellipsis (...) when the Description (in column 4) is 
      # longer than 300 characters  
      list(render = JS("$.fn.dataTable.render.ellipsis(300, true)"), targets = 4)))) %>% 
  # Column specific formatting
  formatStyle("Package", `vertical-align` = "top", `font-family` =  "monospace") %>%
  formatStyle("Function", `vertical-align` = "top", `font-family` =  "monospace") %>%
  formatStyle("Arguments", `vertical-align` = "top", `font-family` =  "monospace") %>% 
  formatStyle("Description", `vertical-align` = "top") %>% 
  formatStyle(6, `font-size` = "20px", cursor = "pointer")

tidyverse_in_a_table

DT::saveWidget(tidyverse_in_a_table, "tidyverse_in_a_table_standalone.html")
