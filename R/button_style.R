#' Add CSS style for buttons
#'
#' @param event \code{character} - What pseudo class or combination of pseudo classes referring to the button. Some choices are 'default' (no pseudo class), 'hover' and 'active'
#' @param button_type \code{character} - What type of button are we talking about. Possible choices are 'browse' and 'radio'.
#'
#' @details This function has no immediate use to end-users. It is just part of the commutability application simplifying the script somewhat relative to tidiness
#'
#' @return \code{character} that contains appropriate CSS style commands for the buttons relative pseudo class
#' @export
#'
#' @examples button_style('default', 'radio')

button_style <- function(event = "default", button_type = "browse"){
  button_class <- ""
  style_body <- ""
  if(length(event) > 1L){
    event <- event[1]
  }
  if(length(button_type) > 1L){
    button_type <- button_type[1]
  }
  if(button_type == "browse"){
    if(event == "default"){
      button_class <- ".btn-default"
      style_body <- "{
                    background-color: #00A2D4;
                    border-color: #008FBB;
                    color: #000000;
                    box-shadow: 0px 0px 6px 2px #FFF inset;
                    border-radius: 8px 8px 8px 8px;
                    transition: box-shadow 0.9s;
                    }"
    }
    else if(event == "hover"){
      button_class <- ".btn-default:hover"
      style_body <- "{
                     background-color: #08C4FF;
                     box-shadow: 0px 0px 8px 5px #FFF inset;
                     border-color: #008FBB;
                     }"
    }
    else if(event == "active"){
      button_class <- ".btn-default:active"
      style_body <- "{
                     background-color: #00A2D4;
                     border-color: #008FBB;
                     box-shadow: 0px 0px 12px 0px #000 inset;
                     }"
    }
    else if(event == "active:hover"){
      button_class <- ".btn-default:active:hover"
      style_body <- "{
                     background-color: #00A2D4;
                     border-color: #008FBB;
                     box-shadow: 0px 0px 15px 0px #000 inset;
                     }"
    }
    else if(event == "focus"){
      button_class <- ".btn-default:focus"
      style_body <- "{
                     background-color: #00A2D4;
                     border-color: #008FBB;
                     box-shadow: 0px 0px 15px 0px #000 inset;
                     }"
    }
  }
  else if(button_type == "radio"){
    if(event == "default"){
      button_class <- ".btn-primary"
      style_body <- "{
                     background-color: #28A745;
                     border-color: #1e7e34;
                     color: #000000;
                     border-radius: 8px 8px 8px 8px;
                     box-shadow: 0px 0px 3px 5px #34ce57 inset;
                     transition: box-shadow 0.5s;
                    }"
    }
    else if(event == "hover"){
      button_class <- ".btn-primary:hover"
      style_body <- "{
                     background-color: #34ce57;
                     border-color: #1e7e34;
                     color: #000000;
                     box-shadow: 0px 0px 5px 6px #5dd879 inset;
                     }"
    }
    else if(event == "active"){
      button_class <- ".btn-primary.active"
      style_body <- "{
                     background-color: #34ce57;
                     border-color: #1e7e34;
                     color: #FFFFFF;
                     font-weight: 900;
                     box-shadow: 0px 8px 37px 10px #0000009E inset;
                     }"
    }
    else if(event == "focus"){
      button_class <- ".btn-primary:focus"
      style_body <- "{
                     background-color: #34ce57;
                     border-color: #000000;
                     border-style: groove;
                     color: #FFFFFF;
                     box-shadow: 0px 8px 37px 10px #0000009E inset;
                     }"
    }
    else if(event == "active:focus"){
      button_class <- ".btn-primary.active.focus"
      style_body <- "{
                     background-color: #34ce57;
                     border-color: #000000;
                     border-style: groove;
                     color: #FFF;
                     box-shadow: 0px 8px 37px 10px #0000009E inset;
                     }"
    }
    else if(event == "active:hover"){
      button_class <- ".btn-primary:active:hover"
      style_body <- "{
                     background-color: #34ce57;
                     border-color: #1e7e34;
                     color: #FFF;
                     box-shadow: 0px 8px 37px 10px #9B000000 inset;
                     }"
    }
    else{
      button_class <- ".btn-primary"
      style_body <- "{
                     background-color: #28A745;
                     border-color: #1e7e34;
                     color: #000000;
                     border-radius: 8px 8px 8px 8px;
                     box-shadow: 0px 0px 3px 5px #34ce57 inset;
                     transition: box-shadow 0.5s;
                    }"
    }
  }

  out <- paste(button_class, style_body)
  return(out)
}
