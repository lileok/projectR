#' set some paths
#'
#' @export
get_dir = function(){
  HOME_drive = ifelse(.Platform$OS.type=="windows","//mymdafiles/usersdqs3/lli11","/home/lli11")
  HN_drive = ifelse(.Platform$OS.type=="windows","//q1prphn/projects/lli11","/projects/lli11")
  Bioinfo2 = ifelse(.Platform$OS.type=="windows", "//d1prpccifs/bcb/bioinfo/bioinfo2", "/data/bioinfo2")
  LUNG_drive = ifelse(.Platform$OS.type=="windows","//q1prplungms/projects","/lungms-projects")
  DataPath = file.path(HN_drive,"Data")
  out = list(HOME_drive=HOME_drive,HN_drive=HN_drive,
             Bioinfo2=Bioinfo2,LUNG_drive=LUNG_drive,DataPath=DataPath)
  invisible(out)
}

#' @export
get_box_dir = function(loc){
  switch (loc,
          office_win = file.path("C:","Users","lli11","OneDrive"),
          home_win = file.path("D:","SkyDrive") ,
          home_mac = file.path("Users","lileok","OneDrive")
          #ubuntu = file.path("~")
  )
}


#' source all dotR files in dir
#' @param dir directory needed source
#' @param  trace show verbose information
#' @export
#'
source_dir = function(dir = getwd(),trace = TRUE, ...){
  for (nm in list.files(dir, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,"sourced")
    source(file.path(dir, nm), ...)
    if(trace) cat("\n")
  }
}


#' Create a new report
#'
#' @title A function to creat new report with required files
#' copied and created to work on
#'
#' @param report_name the name of report
#' @param report_path path of the report. default: getwd()
#' @param echo, logic. Show verbose information
#' @return NA

#' @export
#'

new_report = function(report_name,report_path=getwd(),echo = TRUE){
  report_path = file.path(report_path,report_name)
  if(dir.exists(report_path)) stop("Duplicates folder name found!")
  folders = c("01_indata","02_figures","03_tables","04_rdata","05_html_figures","06_html_caches")
  folder_list = paste(report_path,folders,sep=.Platform$file.sep)
  a = lapply(folder_list,new_folder,echo=FALSE)
  names(a) = folders
  #copy templates files
  #template_folder = file.path(HN_drive,"R2","templates") #path to templates
  template_folder = file.path(system.file(package="projectR"),"extdata","templates") #path to templates
  files_list = paste(template_folder,dir(template_folder),sep=.Platform$file.sep)
  b = file.copy(files_list,report_path)
  names(b) = basename(files_list)
  # rename rmd files
  rmd_name = list.files(report_path,pattern = "*.Rmd",full.names = T)
  c = file.rename(rmd_name,paste(report_path,paste0(report_name,".Rmd"),sep = .Platform$file.sep))
  names(c) = basename(report_name)
  if(echo) list(folder_status = unlist(a),copy_status=b,rename_status=c)
}

new_folder <- function(path, echo=TRUE) {
  if(!file.exists(path)){
    if(echo)cat('New folder created for:\n', path, '\n')
    dir.create(path, recursive=TRUE)
  } else {
    if(echo) cat('Folder already existed for:\n', path, '\n')
  }
}

# import some packages for lazy coding
# function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
#' @export

load_pkgs <- function(...,echo){
  dd = sapply(match.call(), as.character)
  pkgs = dd[-c(1,length(dd))]
  #cat(pkgs,"\n")
  new.pkg = pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  out = data.frame(pkgs=pkgs,loaded_status = sapply(pkgs, require, character.only = TRUE))
  if(echo)out else invisible(out)
}



# set some options for markdown chunk
#' @export
set_global_chunk = function(testReport=T,
                            fig.with = 9,
                            fig.height=6,
                            fig.path = "./05_html_figures/",
                            cache.path = "./06_html_caches/",
                            ...){
  knitr::opts_chunk$set(tidy=TRUE,
                        message=FALSE,
                        warning=FALSE,
                        cache= FALSE,
                        fig.with = fig.with,
                        fig.height = fig.height,
                        echo= testReport,
                        prompt=FALSE,
                        dev=c('png','pdf'),
                        fig.path = fig.path,
                        cache.path = cache.path,...)
}


