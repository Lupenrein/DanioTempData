# Dataset on Temperature Dependency of Zebrafish Early Development 

All data is available in zip compressed form, highres.zip, lowres.zip and a behavior folder with subdirectories according to age and temperature containing the files of the single replicates. All data sets are provided as comma separated plain textvalue files.

## Experiment for high resolution development time series (highres)

Folder highres contains the high-resolution time series data for the early developmental stages, heartbeat onset, hatching and body length.
stages.csv contains one row for each individual embryo in the experiment with columns representing all observed developmental stages. Furthermore, additional columns contain pertinent metadata, such as incubation temperature, replicate number and plate number.
heartbeat.csv and hatching.csv include a column indicating the hpf and a column for each temperature which specifies the number embryos that exhibited an observable heartbeat or had hatched at that point in time. An extra column provides the replicate number.
bodylength_hr.csv has a column indicating the hpf and a column for each well in the plate used for measurements corresponding to one embryo, respectively. The well columns contain the body length of the respective embryo in µm at the associated time point. The meta columns  provide the associated temperature, replicate and plate number. 

## Experiment for low resolution time series (lowres)

The lower resolution time series of body length, yolk sack area, eye area, and swim bladder area can be found in the folderfile lowres.csv. The metadata columns include the following information: hpf, replicate number, and associated temperature.

## Light-Dark Transition Test (behavior)

Folder behavior contains the behavior raw data, which has been divided into two subdirectories by hpf of approximate measurement (96hpf and 120hpf) and again two subdirectories for incubation temperature (26deg and 28deg). The raw data files are designated by a unique batch number. TheyIt contains a table with the following columns: 
Trial_time [s] - the recorded time point at every 0.04 seconds.
Distance_moved [mm] - the swim distance of the embryo in the respective 0.04 seconds.
Distance_to_point [mm] - the distance kept to the center point of the well.
Turn_angle [deg] - the relative angle the embryo turned during the movement.
Individuum - the location in the well plate as a number from left to right and top to bottom in the plate (A1 = 1, H12 = 96).
Temperature – incubation temperature
Unit – unit of temperature
hpf – hours post fertilization 
ID - an individual ID of the embryo in the form w_x_y_z with w = the well position, x = incubation temperature, y = hpf and z = batch number.
