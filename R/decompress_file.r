decompress_file <- function(directory, file, .file_cache = FALSE) {
    if (.file_cache == TRUE) {
        print("decompression skipped")
    } else {

        # Set working directory for decompression
        # simplifies unzip directory location behavior
        wd <- getwd()
        setwd(directory)

        # Run decompression
        decompression <-
            system2("unzip",
                args = c(
                    "-o", # include override flag
                    file
                ),
                stdout = TRUE
            )

        # uncomment to delete archive once decompressed
        file.remove(file)

        # Reset working directory
        setwd(wd)
        rm(wd)

        # Test for success criteria
        # change the search depending on
        # your implementation
        if (grepl("Warning message", tail(decompression, 1))) {
            print(decompression)
        }
    }
}