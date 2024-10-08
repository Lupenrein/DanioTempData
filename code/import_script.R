#Compilation of code for import of raw data
#08/10/2024
#Author: Katja Schroeder
#Affiliation: Institute of Environmental Research RWTH Aaachen 
#Computational Ecotoxicology
#Necessary packages: stringr, gtools
#Developed in R Version: 4.3.3

#1 lowres #########(low-resolution time-series)
###############################################################################

library(stringr)

#files that had measured endpoints in columns and fish individuals in rows were
#imported from a folder rawdata with the name scheme: experiment_xh_ydeg_Rz.csv
#where x = hours post fertilization, y = incubation temperature, z = replicate # 

all_files <- list() #create list to collect dataframes
files <- list.files('rawdata') # read names of files to be imported in 'rawdata'
#loop trough file names and read into list 'all_files'
for (i in files) {
  all_files[[i]] <- read.delim(paste0('rawdata/', i))
}

#loop through the filenames and extract the metainformation from them
for (j in files) {
  #1 extract hours post fertilization (number after first occurance of '_' and 
  #before first occurance of 'h')
  all_files[[j]]$hpf <- str_sub(j, start = str_locate(j, '_')[1]+1,
                                end = str_locate_all(j, 'h')[[1]][2,1]-1)
  #2 extract incubation temperature (number after second occurance of '-' and
  #before first occurance of 'd')
  all_files[[j]]$temperature <- str_sub(j, start = str_locate_all(j,
                                                              '_')[[1]][2,1]+1,
                                        end = str_locate_all(j, 'd')[[1]][1]-1)
  #3 extract replicate number (number after third occurance of '-')
  all_files[[j]]$replicate <- str_sub(j,start = str_locate_all(j,
                                                            '_')[[1]][3,1]+2,
                                      end = str_locate_all(j, '_')[[1]][3,1]+2)
}

#bind all dataframes in the list together to one
total <- do.call(rbind, all_files)
#write to disk
write.csv(total, file = 'lowres.csv', row.names = FALSE, quote = FALSE)


#2 highres ######### high-resolution time-series
###############################################################################

#raw data files that contained the high-resolution time-series were available in
#the form of: 1. column = hpf 2.-last column Well name the corresponding values
#were either body length, time of hatching, time of heartbeat onset or the stage
#file names corresponded to the scheme: x_py_Rz,
#where x = incubation temperature, y = plate number and z = replicate number

# create empty list to collect dataframes
all_files <- list()
files <- list.files('rawdata') # read names of files to be imported in 'rawdata'
#loop trough file names and read dataframes into list
for (i in files) {
  all_files[[i]] <- read.delim(paste0('rawdata/', i))
}


#add replicate number, temperature and plate number from file names
for (j in files) {

  #1 extract replicate number (number after third occurance of '-')
  all_files[[j]]$replicate <- str_sub(j,start = str_locate(j,
                                                           'R')[1]+1,
                                      end = str_locate(j, 'R')[1]+1)
  
  #2 extract incubation temperature (double digit number at the beginning)
  all_files[[j]]$temp <- str_sub(j, start = 1,
                                 end = 2)
  
  #3 extract plate number (number after occurance of 'p')
  all_files[[j]]$plate <- str_sub(j, start = str_locate(j, 'p')[1]+1,
                                end = str_locate(j, 'p')[1]+1)
}

#for the body length data a correction for the water flexion of the well, based
#on a calibration curve was performed for all values (per row r and column c)
for (c in 2:14) {
  total[,c] <- as.numeric(total[,c])
  for (r in 1:nrow(total)) {
    total[r,c] <- total[r,c] * 1.25
  }
}

#bind into one dataframe
total <- do.call(rbind, all_files)
#write to disc
write.csv(total, 'bodylength_hr.csv', row.names = FALSE, quote = FALSE)

#3 behavior ######### behaviour time-series
###############################################################################
# the raw data text files that were exported from EthoVision XT were organized
# in a folder 'data' with the following folder structure for read in:
# tempxhpf/Ry/By.T 
# where x= hours post fertilization, B = batch number and T = temperature
# in every plate folder there was a meta text file with the wellplate layout

library(gtools)

temperature_folders <- list.files('data')

for (t in temperature_folders) {
  
  #initiate list for all dfs
  df_list <- list()
  
  #get all the file names of the sample replicate folders for analysis
  path <- list.dirs(paste0('data/',t), recursive = FALSE)
  
  #loop through all folders 
  for (folder in path) {
    foldersub <- folder ##save foldername
    #open loop through subdirectories (single plates)
    subpaths <- list.dirs(folder)[2:length(list.dirs(folder))]
    sublist <- list()
    for (folder in subpaths) {
      
      file_names <- list.files(folder, pattern = '.txt',
                               full.names = TRUE)
      #sort to ensure read-in of files in the right order (Well after well A1 - H12)
      file_names <- mixedsort(file_names, decreasing = TRUE) 
      #read columns of interest from all raw data files into a list
      file_list <- lapply(file_names,
                          function (i){
                        x <- read.table(i, sep = ';', 
                                                   skip = 40,
                                                   dec = '.', 
                                                   header = FALSE, 
                                                   colClasses = c("numeric",
                                                   "NULL", "NULL", "NULL",
                                                   "NULL", "NULL", "NULL",
                                                   "numeric", "numeric",
                                                   "numeric", "NULL","NULL"),
                                                   na.strings = '"-"')
                  names(x) <- c('Trial_time [s]', c('Distance_moved [mm]',
                                                      'Distance_to_point [mm]',
                                                        'Turn_angle [deg]'))
                          return(x)}
      )
      
      # append fishNr.:
      for (j in 1:length(file_list)) {
        file_list[[j]]['Individuum'] <- c(rep(j, nrow(file_list[[j]])))
      }
      
      #Read in layout metadata  -> filepaths Wellplate layout
      wellplate_file <- list.files(folder, pattern = 'wellplate',
                                   full.names = TRUE)
      
      #read in temperatures and layout, convert data to vector
      wellplate <- as.vector(t((read.csv(wellplate_file, skip = 6, sep = ',',
                                         dec = '.', header = F)))) 
      #remove temperatures (starting with #) and change type to numeric 
      layout <-as.numeric(wellplate[(length(wellplate)-95):length(wellplate)]) 
      
      #Get temperature lines
      #remove plate layout indices (not starting with #)
      temps <- wellplate[!(unlist(lapply(wellplate, function(x) {
        str_detect(x, '#', negate = TRUE)
        
      })))] 

      #extract information about treatment
      #Detects the first '_' and the first space -> subsettets the character(s) 
      #between (start+1 - end -1) which is the temperature
      temp <- unlist(lapply(temps, function(y) {
        str_sub(y, start = (str_locate(y,'_')[1]+1),
                end = str_locate(y,' ')[1]-1)
      })) 
      units <-unlist(lapply(temps, function (y) {
        str_sub(y, start = (str_locate_all(y, ' ')[[1]][1,1]+1),
                end = str_locate_all(y, ' ')[[1]][2,1]-1)
      }))
      temperature <- unlist(lapply(temps, function(y){
        str_sub(y, start = str_locate_all(y, ' ')[[1]][2,1]+1)
      }))
      # bind treatment info into one df 
      temperatures <- t(rbind(temp,units,temperature)) 
      
      ## define hours per fertilization
      if (temperature[1] == 'temp96h') {
        hpf <- 96
      } else hpf <- 119
      
      ## get Batch # and temperature combi from folder names
      replicate_code <- str_sub(folder,
                                start =  str_locate_all(folder, '/')[[1]][
        nrow(str_locate_all(folder, '/')[[1]]),1]+1, end = str_length(folder))
      
      #check if files of fish that are to be removed have been put in the folder
      if (length(na.omit(layout)) == length(file_list)) {
        layout <- na.omit(layout)
      }
      #initiate empty list to save fish that are marked with NA to be removed
      rmv <- vector(mode = 'integer')
      for (m in 1:length(file_list)) { 
        #check that fish is not marked with a 0 in layout to be removed
        if (is.na(layout[m]) != TRUE) {
          #Add missing columns from meta files
          file_list[[m]]$Temperature <- as.vector(
            rep(temperatures[layout[m],1],nrow(file_list[[m]])))
          file_list[[m]]$Temperature_unit <- as.vector(
            rep(temperatures[layout[m],2], nrow(file_list[[m]])))
          file_list[[m]]$hpf <- rep(hpf, nrow(file_list[[m]]))
          
          #create ID containig all metadata
          IDhpf <- paste(file_list[[m]]$hpf[1],'hpf', sep = '')
          file_list[[m]]$ID <- paste(file_list[[m]]$Individuum[1],IDhpf,
                                     replicate_code, sep = '_')
        }
        else rmv <- append(rmv, m)
      }
      #drop fish that are marked with a 0 in layout to be removed
      if (length(rmv) != 0) file_list <- file_list[-rmv]
      
   
        sublist[[folder]] <- do.call(rbind, file_list) 
    }
    
    replicate_code <- str_sub(foldersub, 
                          start =  str_locate_all(foldersub, '/')[[1]][nrow(
                            str_locate_all(foldersub, '/')[[1]]),1]+1,
                          end = str_length(foldersub))
    df <- do.call(rbind,sublist)
    #write to disk
    write.csv(df, paste0('behavior/', t, '/', temp[1], 'deg/',
                        replicate_code, '.csv'),
              row.names = FALSE, quote = FALSE)
  } 
  
}
