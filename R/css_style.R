#' Add CSS style for other R shiny elements
#'
#' @param event \code{character} - What pseudo class or combination of pseudo classes referring to the element. Some choices are 'default' (no pseudo class), 'hover' and 'active'
#' @param element \code{character} - What type of element are we talking about. Some possible choices are 'well' and 'input_field' among others.
#'
#' @details This function has no immediate use to end-users. It is just part of the commutability application simplifying the script somewhat relative to tidiness
#'
#' @return \code{character} that contains appropriate CSS style commands for the given element's relative pseudo class
#' @export
#'
#' @examples css_style('hover', 'progress_bar')

css_style <- function(event = "default", element = "input_field"){
  css_class <- ""
  style_body <- ""
  if(length(event) > 1L){
    event <- event[1]
  }
  if(length(element) > 1L){
    element <- element[1]
  }

  if(element == "input_field"){
    if(event == "default"){
      css_class <- ".form-control"
      style_body <- "{
                     box-shadow: 0px 8px 37px 10px #9BFFFFFF inset;
                     border-radius: 8px 8px 8px 8px;
                     border-color: #008FBB;
                     transition: background-color 0.9s;
                     max-width: 300px;
                     }"
    }
    else if(event == "hover"){
      css_class <- ".form-control:hover"
      style_body <- "{
                     box-shadow: 0px 8px 37px 10px #9BFFFFFF inset;
                     border-radius: 8px 8px 8px 8px;
                     border-color: #008FBB;
                     max-width: 300px;
                     background-color: #ffe7eb;
                     opacity: 0.9;
                     }"
    }
    else if(event == "active"){
      css_class <- ".form-control:active"
      style_body <- "{
                     box-shadow: 0px 8px 37px 10px #9BFFFFFF inset;
                     border-radius: 8px 8px 8px 8px;
                     border-color: #000000;
                     max-width: 300px;
                     background-color: #FFFFFF;
                     opacity: 0.9;
                     }"
    }
    else if(event == "focus"){
      css_class <- ".form-control:focus"
      style_body <- "{
                     box-shadow: 0px 8px 37px 10px #9BFFFFFF inset;
                     border-radius: 8px 8px 8px 8px;
                     border-color: #000000;
                     max-width: 300px;
                     background-color: #FFFFFF;
                     opacity: 0.9;
                     }"
    }
  }
  else if(element == "well"){
    if(event == "default"){
      css_class <- ".well"
      style_body <- "{
                     min-height: 20px;
                     padding: 20px;
                     margin-bottom: 20px;
                     background-color: #F5F5F5;
                     border: 1px solid #E3E3E3;
                     border-radius: 12px;
                     position: relative;
                     box-shadow: inset -3px -2px 6px #000000;
                     width: fit-content;
                     }"
    }
    else if(event == "hover"){
      css_class <- ".well:hover"
      style_body <- "{
                     min-height: 20px;
                     padding: 20px;
                     margin-bottom: 20px;
                     background-color: #FAFAFA;
                     border: 1px solid #e3e3e3;
                     border-radius: 12px;
                     position: relative;
                     width: fit-content;
                     }"
    }
  }
  else if(element == "p"){
    css_class <- ".p, p"
    style_body <- "{
                   font-family: cursive;
                   }"
  }
  else if(element == "h"){
    css_class <- ".h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6"
    style_body <- "{
                   font-family: cursive;
                   color: #605CA8;
                   }"
  }
  else if(element == "b"){
    #5
  }
  else if(element == "progress_bar"){
    if(event == "default"){
      css_class <- ".progress"
      style_body <- "{
                     background:repeating-linear-gradient(-45deg, #ffffff 3%, #ededed 8%);
                     border-radius: 1px 8px 1px 8px;
                     box-shadow: 10px 10px 16px 2px #b9b9b9;
                     transition: box-shadow 0.3s;
                     }"
    }
    else if(event == "hover"){
      css_class <- ".progress:hover"
      style_body <- "{
                     background:repeating-linear-gradient(-45deg, #ffffff 3%, #ededed 8%);
                     border-radius: 1px 8px 1px 8px;
                     box-shadow: 10px 10px 7px 2px #b9b9b9;
                     transition: box-shadow 0.3s;
                     }"
    }
    else if(event == "bar"){
      css_class <- ".progress-bar"
      style_body <- "{
                     color: #fff;
                     font-weight: 500;
                     font-size: 15px;
                     text-align: center;
                     background-color: #28a745;
                     transition: width .9s cubic-bezier(.8,.15,1,1);
                     }"
    }
  }
  else if(element == "rectangle"){
    if(event == "default"){
      css_class <- ".rectangle"
      style_body <- "{
                     width: fit-content;
                     background-color: #28A745;
                     border-color: #1e7e34;
                     color: #000;
                     border-radius: 8px 8px 8px 8px;
                     box-shadow: 0px 0px 3px 3px #34ce57 inset;
                     transition: box-shadow 0.9s;
                     display: flex;
                     align-items: center;
                     justify-content: center;
                     }"
    }
    else if(event == "hover"){
      css_class <- ".rectangle:hover"
      style_body <- "{
                     width: fit-content;
                     background-color: #34ce57;
                     border-color: #1e7e34;
                     color: #000;
                     box-shadow: 0px 0px 5px 6px #5dd879 inset;
                     display: flex;
                     align-items: center;
                     justify-content: center;
                     }"
    }
    else if(event == "p"){
      css_class <- ".rectangle p"
      style_body <- "{
                     line-height: 2em;
                     margin: 0;
                     padding-left: 0.5em;
                     padding-right: 0.5em;
                     }"
    }
  }
  out <- paste(css_class, style_body)
  return(out)
}
