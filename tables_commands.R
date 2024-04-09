mypostprocess = function(x,onlyinner=TRUE,addfilename=TRUE ,comment_out_clutter=TRUE, add_cmidrule = TRUE){
    if(comment_out_clutter){
        qui_se = grep("standard-errors in parentheses", x)
        qui_significance = grep("Signif. Codes", x)
        
        x[qui_se] = paste0("%",x[qui_se])
        x[qui_significance]= gsub("\n","\n%",x[qui_significance])
        
        class(x) = "etable_tex"
    }
    
    if(onlyinner){
        qui_start = grep("begin\\{tabular", x)
        qui_end = grep("end\\{tabular", x)
        x = (x[(qui_start+1):(qui_end-1)])
        class(x) = "etable_tex"
    }
    
    if(addfilename){
        current_file <- as.character(sys.frames()[[1]]$ofile)
        if (is.null(current_file) | length(current_file)==0 ) {
            current_path = rstudioapi::getActiveDocumentContext()$path 
            current_file = gsub(".*/","",current_path)
        }
        x[length(x)+1] = paste0("% This table produced by the following file: \"",current_file,"\" on: ",Sys.Date()," \n ")
    }
    
    if (add_cmidrule) {
        
        header_idx <- grep("Dependent Variable", x) # Find the index of the line with the header information
        
        # Check if the header contains a multicolumn
        if (grepl("multicolumn", x[header_idx])) {
            # Split the header line into separate columns
            header_cols <- unlist(strsplit(x[header_idx], split = "&"))
            
            # Find which columns have a multicolumn
            has_multi <- grepl(pattern = "\\\\multicolumn", x = header_cols)
            
            # Calculate the width of each multicolumn and the indices of the columns
            # they span
            col_widths <- numeric()
            for (i in seq_along(has_multi)) {
                if (!has_multi[i]) {
                    col_widths <- c(col_widths, 1)
                } else {
                    multi_col_width <- gsub("^.*?([0-9]+).*", "\\1", header_cols[i])
                    col_widths <- c(col_widths, as.numeric(multi_col_width))
                }
            }
            col_ends <- cumsum(col_widths)
            col_starts <- col_ends - col_widths + 1
            
            # Remove the first element, as I don't want "dep var" to have \cmidrule
            col_starts <- col_starts[-1]
            col_ends <- col_ends[-1]
            
            # Create the \cmidrule commands
            cmidrule_cmds <- character()
            for (i in seq_along(col_starts)) {
                cmidrule_cmds <- c(cmidrule_cmds, paste0(" \\cmidrule(lr){", col_starts[i], "-", col_ends[i], "}"))
            }
            cmidrule_cmds <- paste0(cmidrule_cmds, collapse = " ")
            
            # Insert the \cmidrule commands into the header line
            x[header_idx] <- paste0(x[header_idx], cmidrule_cmds)
            
            # Set the class of the object to indicate that it is a table in LaTeX format
            class(x) <- "etable_tex"
        }
    }
    
    
    x
}


merge_tables <- function(file_paths, labels,filename=NULL) {
    # Load the necessary packages
    require(stringr)
    
    create_table_label <- function(panel_name, label_text, file_num, num_cols) {
        label <- paste0("\\multicolumn{", num_cols, "}{l}{\\textbf{", panel_name, ": ", label_text[file_num], "}} \\\\")
        return(label)
    }
    
    
    # Find the number of files to merge
    num_files <- length(file_paths)
    
    # Read the contents of each file and find the number of columns in each table
    file_contents <- vector("list", num_files)
    file_num_cols <- vector("integer", num_files)
    
    for (i in 1:num_files) {
        file_contents[[i]] <- readLines(file_paths[i])
        file_num_cols[i] <- max(str_count(file_contents[[i]], "&")) + 1
    }
    
    # Check if all the tables have the same number of columns
    if (length(unique(file_num_cols)) > 1) {
        stop("The tables have different numbers of columns.")
    }
    
    # Find the row index of "Dependent Variable" in each file
    file_depvar_rows <- sapply(file_contents, function(x) grep("Dependent Variable", x))
    
    # Extract the relevant lines from each file based on the line numbers
    depvar_rows <- sapply(seq_along(file_contents), function(i) {
        file_contents[[i]][file_depvar_rows[i]]
    })    
    depvar_rows <- gsub("\\s+", "", depvar_rows)
    if(all(depvar_rows == depvar_rows[1])) {
        # Files have the same dependent variable, add panel labels after coefficient part
        for (i in seq_along(file_contents)) {
            coeff_row <- grep("%Coefficients ", file_contents[[i]])
            file_contents[[i]] <- append(file_contents[[i]], create_table_label(paste0("Panel ", LETTERS[i]), labels[i], num_cols = file_num_cols[i]), after = coeff_row)
        }
    } else {
        # Files have different dependent variables, add panel labels before dependent variable row
        for (i in seq_along(file_contents)) {
            file_contents[[i]] <- append(file_contents[[i]], create_table_label(paste0("Panel ", LETTERS[i]), labels[i], num_cols = file_num_cols[i]), after = file_depvar_rows[[i]] - 1)
        }
    }    
    # Comment out everything before Panel title in each file except for the first one
    for (i in 2:num_files) {
        panel_row <- grep(paste0("Panel ", LETTERS[i], ":"), file_contents[[i]])
        file_contents[[i]][1:(panel_row - 1)] <- paste0("%", file_contents[[i]][1:(panel_row - 1)])
    }
    
    # Comment out the lines after the FE part in each file
    file_fe_rows <- sapply(file_contents, function(x) grep("%FE part", x))
    for (i in 1:num_files) {
        # if
        if(length(file_fe_rows[[i]]) == 0) {
            next # skip current iteration of the loop
        }
        # Comment out the lines after the FE part in the last file
        file_contents[[i]][(file_fe_rows[i]):length(file_contents[[i]])] <- paste0("%", file_contents[[i]][(file_fe_rows[i]):length(file_contents[[i]])])
    }    
    
    #Combine the files into one table
    combined_table <- unlist(file_contents)
    
    # Remove % from last end tabular 
    if (sum(grepl("^\\\\begin\\{tabular\\}", combined_table))>0) {
        last_end_tabular <- max(grep("\\\\end\\{tabular\\}", combined_table))
        combined_table[last_end_tabular] <- sub("%", "", combined_table[last_end_tabular])
    }
    
    # Remove % from endgroup in last file
    if (sum(grepl("^\\\\begingroup", combined_table))>0) {
        last_end_group <- max(grep("\\\\endgroup", combined_table))
        combined_table[last_end_group] <- sub("%", "", combined_table[last_end_group])
        
    }
    
    combined_table <- subset(combined_table, !grepl("^\\s*$|^%*$", combined_table))
    
    texfile_name<-filename
    if (is.null(filename)) {
        texfile_name<-paste0(paste(gsub(".tex","",file_paths),collapse = "_"),".tex")
    } 
    
    write(combined_table, file = texfile_name)
    
}


#merge_tables(file_paths = c("panelA.tex","panelB.tex","panelC.tex"),labels = c("All data","All data2","large values"))


comment_out_tex_lines <- function(file_path, search_strings) {
    # Read the file
    lines <- readLines(file_path)
    
    # Search for lines containing the search strings
    lines_to_comment <- c()
    for (string in search_strings) {
        lines_to_comment <- c(lines_to_comment, grep(string, lines))
    }
    
    # Comment out the lines
    lines[lines_to_comment] <- paste0("%", lines[lines_to_comment])
    
    # Write the modified file back to disk
    writeLines(lines, file_path)
}



uncomment_tex_lines <- function(file_path, uncomment_strings) {
    # Read the file
    lines <- readLines(file_path)
    
    # Uncomment the lines
    for (uncomment_string in uncomment_strings) {
        lines_to_uncomment <- grep(paste0("^%*.*", uncomment_string), lines)
        lines[lines_to_uncomment] <- gsub("^%", "", lines[lines_to_uncomment])
    }
    
    # Write the modified file back to disk
    writeLines(lines, file_path)
}


