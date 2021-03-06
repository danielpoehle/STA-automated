# Contents
# . Helper Functions for File Handling
# . Helepr Functions for



# create Folder if not existing
helper.safeCreateFolder = function (path) {
  path_points = unlist(strsplit(path, "[/]"))
  current_path = ''
  for (pp in path_points) {
    current_path = paste0(current_path, pp, "/")
    if (!dir.exists(current_path)) {
      dir.create(current_path)
    }
  }

}

# Create result directory if necessary
RESULT_PATH = here(paste0("result_detail_v", RESULT_VERSION, "/"))
if (!dir.exists(RESULT_PATH)) {
  dir.create(RESULT_PATH)
}

helper.getResultPath = function (filename) {
  return (paste0(RESULT_PATH, filename))
}

# Progressbar Functions
#helper.log_socket = helper.getResultPath("log.txt")
helper.log = function(msg) {
  print_message = sprintf(paste0(as.character(Sys.time()), ": ", msg, "\n"))
  cat (print_message)

}

# Check if File Exists, Raise Error if not (error_message = additional error message, if file not found)
helper.requireFile = function (path, error_message = "") {
  if (! file.exists(path)) {
    stop(paste0(path, " not found ! - Please set working directory. ", error_message))
  }
}

helper.included = T
