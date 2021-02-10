#### Incremental sampling code - Author: Leonardo Mazzaferro ####

####LIBRARIES####
library(caret)
library(corrplot)
library(data.table)
library(deldir) #voronoi diagrams
library(DiceDesign) #discrepancy
library(doParallel)
library(dplyr) #use for fixing up data
library(GGally)
library(ggplot2)
library(ggmap) #ggplot functionality for maps
library(grid)
library(gridExtra)
library(hexbin)
library(Hmisc)
library(hydroGOF)
library(hydroTSM)
library(lhs)
library(magick) #animate png files
library(mxnet)
library(NeuralNetTools)
library(plyr)
library(pse)
library(purrr) #mapping over a function
library(randtoolbox)
library(readr) #reading in data/csv
library(RColorBrewer) #color palettes
library(Rmisc)
library(rlist)
library(sampling)
library(scales)
library(snow)
library(viridis) #more color palettes


#### DEFINE AND SET PATHS ####
path_incremental <- "C:/Users/LabEEE_1-3/Desktop/incremental"
path_sample <- "C:/Users/LabEEE_1-3/Desktop/incremental/sample_creation"

setwd(path_sample)

generator <- 69 
#lembrar de melhorar a descricao de itens comuns nas zonas perimetrais mas ausentes nas centrais (ex.: abs_extwall, abs_roof, horiz_b_d, shading, sur_abs, sur_wwr, wwr, delta_nfloor)!!!!!!!!!!!!!!!

#### INCREMENTAL SAMPLING HYPER-PARAMETERS ####
set.seed(generator) #reproducible results

hyp <- list()
hyp$starter_points <- 20 #minimum required: 2 (for sampling) | number of dimensions (for discrepancy)
hyp$incremental_points <- 20
hyp$final_points <- 400
hyp$extra_points <- 0 #edit here in case of continued experiment
hyp$n_iter <- (hyp$final_points - hyp$starter_points)/hyp$incremental_points #set maximum iterations
hyp$dimensions_number <- NA #defined afterwards: numerical variables + categorical variables
hyp$n_climates_per_iter <- 1 #pick n climates from the pre-defined interval
hyp$fps <- 0.5 #frames per second for animation

graph <- list()
graph$alpha <- 0.1 #lower values help visualization in high density plots
graph$dot_size <- 0.8

#graphic parameters
axis_min <- -5
axis_max <- 100

#remember to also change the sampling method in whole_building_generator.py file
sampling_method <- "Sobol"

### TRANSFER LEARNING ###
transfer_learning <- TRUE # use transfer learning in models or not

### CONTINUE OR NOT ###
continue_iter <- FALSE # reset or continue from where it has stopped

if (continue_iter == FALSE) {
  
  #reset iteration count
  iteration <- 0
  #print info
  print(paste0("This is a new experiment and it will start from iteration number: ", iteration))
  #set string patterns to clean folder
  png_files_in_dir <- dir(path = path_sample, pattern = "*.png")
  gif_files_in_dir <- dir(path = path_sample, pattern = "*.gif")
  csv_files_in_dir <- dir(path = path_sample, pattern = "*.csv")
  idf_files_in_dir <- dir(path = path_sample, pattern = "caso_*")
  
  
  #delete files with those string patterns
  file.remove(file.path(path_sample,
                        png_files_in_dir, gif_files_in_dir,
                        csv_files_in_dir, idf_files_in_dir))
  
  #remove strings patters
  rm(png_files_in_dir, gif_files_in_dir, csv_files_in_dir, idf_files_in_dir)
  
}


if (continue_iter == TRUE) {
  
  #set iteration count
  iteration <- length(dir(path = path_incremental, pattern = "iter_*"))
  #print info
  print(paste0("The experiment will continue from iteration number: ", iteration))
  #set a new number of maximum iterations
  hyp$n_iter <- (hyp$final_points + hyp$extra_points - hyp$starter_points)/hyp$incremental_points
  
  #add more steps                 MAKE IT WORK 1 !!!!!!!!!!!!!!!!!!!
  #if false go ahead, if true read sampling files and read dataframe
  
}


### Scatterplot matrix FUNCTIONS ###
upper_function <- function(data,mapping){
  ggplot(data = data, mapping = mapping) +
    geom_density2d(colour="black") +
    #stat_density2d(aes(fill=..density..), geom="tile", contour = TRUE) +
    #scale_fill_gradientn(colours=rainbow(100)) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1))
}

lower_function <- function(data,mapping){
  ggplot(data = data, mapping = mapping) +
    geom_point(size=0.3) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1))
}


#R and p-value FUNCTION (for scatterplot matrix)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient (examine the linear relationship between variables)
  # range from -1 to 1
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(x=0.5, y=0.6, cex=1.5, txt)
  
  # p-value calculation (determine whether the correlation coefficient is significant)
  # e.g. alpha=5% ... risk of concluding that a correlation exists when actually no correlation exists
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(x=0.5, y=0.4, cex=1.5, txt2)
}


### Factor to numeric conversion FUNCTION ###
asNumeric <- function(x) as.numeric(as.character(x))

factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))
### Move to first FUNCTION ###
movetofirst <- function(data, move) {data[c(move, setdiff(names(data), move))]}


#### BUILDING PARAMETERS ####
#choose building parameters to be sampled

##NUMERIC & VARIABLE
nv_building_xlen <- c(14,50) #19
nv_ceiling_height <- c(2.4,4.8)
nv_nfloor <- c(1,4) #"floor" function rounds down these numbers... c(1,4) equals to 1-3
nv_shading <- c(0,2) #num with conditional, no shading=0, 1m=1, 2m=2

nv_abs_roof_construction <- c(0.10,0.99) #(0.15,0.95) #describe it only in TOP floor zones of df4!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nv_abs_extwall_construction <- c(0.10,0.99) #(0.15,0.95)
nv_wwr <- c(0.01,0.99)

nv_people <- c(0,0.5) #c(0.05,0.25)
nv_lights <- c(0,24) #c(4,20)
nv_equip <- c(0,24) #c(4,20)

nv_horizontal_building_density <- c(0.01,0.49) #0.25 turns the space between buildings equals to facade length
nv_sur_nfloor <- c(1,5) #"floor" function rounds down these numbers... c(1,5) equals to 1-4 #minimum must be 1 due tp "ceiling" function in idf creation and data proc
nv_sur_abs_extwall <- c(0.1,0.9)
nv_sur_wwr <- c(0.1,0.9)

nv_thermostat <- c(17,26)


list_nv_vars <- mget(ls(pattern = "^nv_"))
number_of_nv_vars <- length(list_nv_vars)

var_min <- as.vector(sapply(list_nv_vars, min, simplify = TRUE))
var_max <- as.vector(sapply(list_nv_vars, max, simplify = TRUE))
var_range <- var_max - var_min


##NUMERIC & CONSTANT (won't change but they are needed to create the sample file)
nc_building_ratio <- 1 #c(1,3)
nc_building_block_position <- 0 #building in grid center = 0
nc_shell_depth <- 4.5
nc_azimuth <- 0 #from 0 to 179? (slices of 15? ... eventually)
nc_sur_vis_transmittance <- 0 #0=opaque, 1=100% translucid surroundings ...maybe set 80% = 0 and 20% = 1 distribution
nc_shading_ceiling_height <- 1 #0=sombreamento na janela; 1=sombreamento na altura do pe direito (sacada)
nc_pilotis <- 0 #round in idf creation/data proc: 0=no pilotis, 1=pilotis; describe it only in BOTTOM floor zones of df4!!!!!!!!!!!!!!!!!!!!


list_nc_vars <- mget(ls(pattern = "^nc_"))
number_of_nc_vars <- length(list_nc_vars)
vector_nc <- list.cbind(list_nc_vars)


##CATEGORICAL & VARIABLE
cv_schedule <- c("SCH_08H","SCH_10H","SCH_12H","SCH_14H","SCH_16H")
cv_extwall_construction <-  c("Parede11","Parede12","Parede13","Parede21","Parede22","Parede23","Parede31","Parede32","Parede33")
cv_roof_construction <- c("Cobertura11","Cobertura12","Cobertura13","Cobertura21","Cobertura22","Cobertura23","Cobertura31","Cobertura32","Cobertura33")
cv_glass_construction <- c("Vid1","Vid2") #FS(Vid1)=0.38 -6mm; FS(Vid2)=0.82 -6mm

list_cv_vars <- mget(ls(pattern = "^cv_"))
number_of_cv_vars <- length(list_cv_vars)
list_cv_levels <- lapply(list_cv_vars, length)


##CATEGORICAL & CONSTANT #MAKE IT WORK 2 - CONSTRUCTION COMPONENTS!!!!!!!!!!!!!!!!!!!!!!!!!!!
cc_floor_construction <- "PisoCeramica" #"PisoMadeira"
cc_sur_glass <- "Vid2" #FS(Vid1)=0.38 -6mm; FS(Vid2)=0.82 -6mm

#cc_interior_blind <- "AlwaysOff" #thinking about including this variable


list_cc_vars <- mget(ls(pattern = "^cc_"))
number_of_cc_vars <- length(list_cc_vars)
vector_cc <- list.cbind(list_cc_vars)
#rm(list_NV=ls(pattern="^nv_"), list_NC=ls(pattern="^nc_"), list_CV=ls(pattern="^cv_"), list_CC=ls(pattern="^cc_"))

#number of dimensions
hyp$dimensions_number <- number_of_cv_vars + number_of_nv_vars


#### SAMPLING METHODS ####
methods_vector <- c("Random LHS"
                    , "Improved LHS"
                    , "Optimum LHS" #remove when the sampling procedure is too slow
                    , "Sobol"
                    , "Halton"
                    , "Torus"
)

number_of_methods <- length(methods_vector)


#### INCREMENTAL SAMPLING IMPLEMENTATION ####

#list of sample matrices
ls_sample_mat <- list()

#list of each method discrepancy types (CL2, L2, etc)
ls_discr_types <- list()

# #dataframe with discrepancy evolution ... CAUTION: number of points cannot be lower than number of dimensions
df.discr <- data.frame(matrix(0, ncol = number_of_methods, nrow = hyp$n_iter + 1))
colnames(df.discr) <- methods_vector
df.discr$Iteration <- 0:hyp$n_iter
df.discr$SampleSize <- seq(from = hyp$starter_points,
                           to = hyp$final_points,
                           by = hyp$incremental_points)

#set discrepancy type
discrepancy_type <- "C2"
discrepancy_type_name <- " "
discrepancy_type_name <- ifelse(discrepancy_type == "L2", discrepancy_type_name <- "L2", discrepancy_type_name)
discrepancy_type_name <- ifelse(discrepancy_type == "C2", discrepancy_type_name <- "Centered L2", discrepancy_type_name)

#reproducible results
set.seed(generator)

for (h in 1:number_of_methods) {
  
  sampling_method <- methods_vector[[h]]
  print(sampling_method)
  
  if (sampling_method == "Random LHS") {
    data <- randomLHS(n=hyp$starter_points,
                      k=hyp$dimensions_number,
                      preserveDraw=FALSE)
    
    df.discr$`Random LHS`[1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
  }
  
  
  if (sampling_method == "Improved LHS") {
    data <- improvedLHS(n=hyp$starter_points,
                        k=hyp$dimensions_number,
                        dup=1)
    
    df.discr$`Improved LHS`[1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
  }
  
  
  if (sampling_method == "Optimum LHS") {
    data <- improvedLHS(n=hyp$starter_points,
                        k=hyp$dimensions_number)
    
    df.discr$`Optimum LHS`[1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
  }
  
  
  if (sampling_method == "Sobol") {
    data <- sobol(n=hyp$starter_points,
                  dim=hyp$dimensions_number,
                  init=TRUE,
                  scrambling=0,
                  seed=generator,
                  normal=FALSE)
    
    df.discr$`Sobol`[1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
  }
  
  
  if (sampling_method == "Halton") {
    data <- halton(n=hyp$starter_points,
                   dim=hyp$dimensions_number,
                   init=TRUE,
                   normal=FALSE,
                   usetime=FALSE)
    
    df.discr$`Halton`[1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
  }
  
  
  if (sampling_method == "Torus") {
    data <- torus(n=hyp$starter_points,
                  dim=hyp$dimensions_number,
                  init=TRUE,
                  mixed=FALSE,
                  normal=FALSE,
                  usetime=FALSE)
    
    df.discr$`Torus`[1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
  }
  
  
  
  #for each iteration (starter points + incremental points, until the final points are reached)
  for (i in 0:hyp$n_iter) {
    
    current_points <- hyp$starter_points + i*hyp$incremental_points
    print(current_points)
    
    if (i != 0 & sampling_method == "Random LHS") {
      data <- augmentLHS(data,
                         m=hyp$incremental_points)
      
      df.discr$`Random LHS`[i+1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
      ls_sample_mat[[h]] <- data
      names(ls_sample_mat)[[h]] <- methods_vector[h]
    }
    
    
    if (i != 0 & sampling_method == "Improved LHS") {
      # data <- optAugmentLHS(data,
      #                       m=hyp$incremental_points,
      #                       mult=hyp$dimensions_number)
      data <- augmentLHS(data,
                         m=hyp$incremental_points) #testing
      
      df.discr$`Improved LHS`[i+1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
      ls_sample_mat[[h]] <- data
      names(ls_sample_mat)[[h]] <- methods_vector[h]
    }
    
    
    if (i != 0 & sampling_method == "Optimum LHS") {
      # data <- optSeededLHS(data,
      #                      m=hyp$incremental_points,
      #                      maxSweeps = 2,
      #                      eps = .1)
      data <- augmentLHS(data,
                         m=hyp$incremental_points) #testing
      
      df.discr$`Optimum LHS`[i+1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
      ls_sample_mat[[h]] <- data
      names(ls_sample_mat)[[h]] <- methods_vector[h]
    }
    
    #if augmentLHS works as I think... I can add maximinLHS and geneticLHS to sampling methods analysis!!!!!!!!
    
    
    if (i != 0 & sampling_method == "Sobol") {
      new_data <- sobol(n=hyp$incremental_points,
                        dim=hyp$dimensions_number,
                        init=FALSE,
                        scrambling=0,
                        seed=generator,
                        normal=FALSE)
      
      data <- rbind(data, new_data)
      
      df.discr$`Sobol`[i+1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
      ls_sample_mat[[h]] <- data
      names(ls_sample_mat)[[h]] <- methods_vector[h]
    }
    
    
    if (i != 0 & sampling_method == "Halton") {
      new_data <- halton(n=hyp$incremental_points,
                         dim=hyp$dimensions_number,
                         init=FALSE,
                         normal=FALSE,
                         usetime=FALSE)
      
      data <- rbind(data, new_data)
      
      df.discr$`Halton`[i+1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
      ls_sample_mat[[h]] <- data
      names(ls_sample_mat)[[h]] <- methods_vector[h]
    }
    
    
    if (i != 0 & sampling_method == "Torus") {
      new_data <- torus(n=hyp$incremental_points,
                        dim=hyp$dimensions_number,
                        init=FALSE,
                        mixed=FALSE,
                        normal=FALSE,
                        usetime=FALSE)
      
      data <- rbind(data, new_data)
      
      df.discr$`Torus`[i+1] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
      ls_sample_mat[[h]] <- data
      names(ls_sample_mat)[[h]] <- methods_vector[h]
    }
    
    
    n.points <- sprintf("%05d", current_points) #add zeros before number
    image.name <- paste0(sampling_method," ",n.points," points",".png")
    
    df.data <- as.data.frame(data)
    colnames(df.data) <- c(paste0("var",1:hyp$dimensions_number))
    
    # #GGPLOT plots with sampling methods distributions
    # inc.plot <- ggpairs(data = df.data,
    #              title = paste(sampling_method,"/ iteration",
    #                            i,"/ number of points:",n.points),
    #              upper = list(continuous = "cor", combo = "box_no_facet"),
    #              diag=list(continuous = "densityDiag"),
    #              lower = list(continuous = wrap(lower_function)),
    #              axisLabels = "none")
    # 
    # print(inc.plot)
    # ggsave(filename = image.name,
    #        plot = last_plot(),
    #        scale = 1,
    #        width = 10,
    #        height = 10,
    #        units = "in")
    # 
    # dev.off()
    
  }
  
  #} #temporary
  
  
  # # MODULE 2: List GGPLOT plots with sampling methods distributions, read them in, and then make animation
  # list.files(path = path_sample,
  #            pattern = paste0("^",sampling_method),
  #            full.names = T) %>%
  # 
  #   map(image_read) %>% # reads each path file
  #   image_join() %>% # joins images
  #   image_animate(fps=hyp$fps) %>%
  #   image_write(paste0("animated incremental ",
  #                      sampling_method,".gif"))
  
  
  ### MODULE 3: write csv files for each "raw" sampling method
  csv_data <- ls_sample_mat[[h]]
  iteration_vector <- c(rep(0,each = hyp$starter_points), 
                        rep(1:hyp$n_iter, each = hyp$incremental_points))
  
  csv_data <- cbind(csv_data, iteration_vector)
  mat_colnames <- sprintf("v%d",seq(1:hyp$dimensions_number))
  
  colnames(csv_data) <- c(mat_colnames, "it")
  write.csv(csv_data,
            file = paste0("sample raw ",sampling_method,".csv"),
            row.names = FALSE)
  
}


### MODULE 4: Calculate discrepancy and plot the optimal methods for each sample size
#(that will probably be validated by NN performance afterwards) #voronoi tesselation diagrams
x <- cbind(df.discr$SampleSize,
           df.discr$SampleSize,
           df.discr$SampleSize, #removed... sampling was too slow!!!
           df.discr$SampleSize,
           df.discr$SampleSize,
           df.discr$SampleSize)

y <- cbind(df.discr$`Random LHS`,
           df.discr$`Improved LHS`,
           df.discr$`Optimum LHS`, #removed... sampling was too slow!!!
           df.discr$Sobol,
           df.discr$Halton,
           df.discr$Torus)

matplot(x,y,
        type="p",
        xlab = "Sample Size", 
        ylab = paste0(discrepancy_type_name, " discrepancy for ", hyp$dimensions_number, " dimensions"))

#find tesselation coding file (I think I wrote one hehe) and try to implement it here!
#to generate tesselation plots?    MAKE IT WORK 3 !!!!!!!!!!!!!!!!!!!!!!


### MODULE 5: Climate sampling

#calculate number of needed climates
number_of_needed_climates <- hyp$n_climates_per_iter*(hyp$n_iter + 1)

#create vector with iteration number
climate_iter <- rep(x = 0:hyp$n_iter, each = hyp$n_climates_per_iter)

#sample climates in a balanced approach, using ANOTHER sampling method :)
set.seed(generator)

climate_raw_sample <- randomLHS(n = number_of_needed_climates,
                                k = 1,
                                preserveDraw=FALSE)

# climate_raw_sample <- sobol(n = number_of_needed_climates,
#                             dim = 1, init = TRUE, scrambling = 0,
#                             seed = generator, normal = FALSE)

print(climate_raw_sample)

#climate raw sampling... just dots
plot(climate_raw_sample)


climate_id <- seq(1:length(climate_raw_sample))

#climate_df <- data.frame(climate_id,climate_raw_sample,climate_iter)


#load monthly mean values of climatic variables
df_monthly_mean_backup <- read.csv(paste0(path_incremental,"/df_monthly_mean.csv"))

#subset just the wanted climatic variable
df_wanted_climatic_variable <- df_monthly_mean_backup[ ,c(7,8,1)]
colnames(df_wanted_climatic_variable) <- c("ID", "MDBT", "ID_Month")

#calculate the mean value and std deviation of each climate (using the monthly mean values)
df_mean_of_mean <- aggregate(. ~ ID, data = df_wanted_climatic_variable[ ,1:2], 
                             FUN = function(x) mn = mean(x))

df_sd_of_mean <- aggregate(. ~ ID, data = df_wanted_climatic_variable[ ,1:2], 
                           FUN = function(x) sd = sd(x))

df_mean_sd_of_mean <- cbind(df_mean_of_mean, df_sd_of_mean[ ,2])

#remove extra files
#rm(df_mean_of_mean, df_sd_of_mean, df_wanted_climatic_variable, df_monthly_mean_backup)

#rename columns
colnames(df_mean_sd_of_mean) <- c("ID","MMDBT","SMDBT")


#convert factor to character
df_mean_sd_of_mean$ID <- as.character(df_mean_sd_of_mean$ID)

#"a" is the range (max-min); "b" is the starting value (min); "x" is the sampling value (from 0 to 1)
a_climate <- max(df_mean_sd_of_mean$MMDBT) - min(df_mean_sd_of_mean$MMDBT)
b_climate <- min(df_mean_sd_of_mean$MMDBT)

#convert raw sample to the climatic variable value, using ranges by a transformation function (c = a*x + b)
c_climate <- a_climate*climate_raw_sample + b_climate

#create empty vectors
ID <- c(rep("NA", times = number_of_needed_climates))
MMDBT <- c(rep("NA", times = number_of_needed_climates))

#find climate with the closest absolute mean value to sample
for (clima in 1:number_of_needed_climates) {
  
  print(paste("Number of climate in sampling:", clima))
  
  ID[clima] <- df_mean_sd_of_mean$ID[which.min(abs(df_mean_sd_of_mean$MMDBT - c_climate[clima]))]
  ID[clima] <- paste0(ID[clima],".epw")
  print(ID[clima])
  
  MMDBT[clima] <- df_mean_sd_of_mean$MMDBT[which.min(abs(df_mean_sd_of_mean$MMDBT - c_climate[clima]))]
  print(paste("Mean monthly dry bulb temperature:", MMDBT[clima]))
  
}


#build a dataframe of climate sampling
df_climate_sample <- data.frame(climate_id,climate_iter, climate_raw_sample,c_climate,MMDBT,ID)
df_climate_sample$MMDBT <- round(as.numeric(as.character(df_climate_sample$MMDBT)), digits = 2)
df_climate_sample$ID <- as.character(df_climate_sample$ID)
df_climate_sample$ID <- gsub(".epw", "", df_climate_sample$ID)


#merge dataframes
df_climate_sample2 <- merge(df_climate_sample, df_wanted_climatic_variable)
#order dataframe by ID_Month column
df_climate_sample2 <- df_climate_sample2[order(df_climate_sample2$ID_Month), ] 
#reset rownames in dataframe
rownames(df_climate_sample2) <- NULL
#re-add ".epw" to climate ID column in both dataframes
df_climate_sample$ID <- paste0(df_climate_sample$ID,".epw")
df_climate_sample2$ID <- paste0(df_climate_sample2$ID,".epw")


#climate raw sampling in ggplot
climate_raw_sampling_plot <- ggplot(df_climate_sample, 
                                    aes(x = climate_id, y = climate_raw_sample, 
                                        color = as.factor(climate_iter))) + 
  geom_point(shape=8, size=3) +
  xlab("Quantidade de climas") +
  ylab("Amostragem bruta") +
  labs(colour = "Iteração") +
  #scale_x_continuous(breaks= pretty_breaks()) +
  xlim(min(climate_id),max(climate_id)) + ylim(0,1) +
  theme(legend.position="bottom")

print(climate_raw_sampling_plot)

ggsave(filename = paste0("climate_raw_sampling_plot.png"),
       plot = last_plot(),
       scale = 1,
       width = 5,
       height = 6,
       units = "in")
dev.off()


#climate DBT sampling in ggplot - yearly
climate_dbt_year_sampling_plot <- ggplot(df_climate_sample, 
                                         aes(x = climate_id, y = MMDBT, 
                                             color = as.factor(climate_iter))) + 
  geom_point(shape=19, size=3) +
  xlab("Quantidade de climas") +
  ylab("Média das médias mensais de temperatura de bulbo seco (°C)") +
  labs(colour = "Iteração") +
  xlim(min(climate_id),max(climate_id)) + ylim(7,30) +
  #scale_y_continuous(position = "right") +
  geom_hline(yintercept = max(df_mean_sd_of_mean$MMDBT), linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = min(df_mean_sd_of_mean$MMDBT), linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position="bottom")

print(climate_dbt_year_sampling_plot)

ggsave(filename = paste0("climate_dbt_year_sampling_plot.png"),
       plot = last_plot(),
       scale = 1,
       width = 5,
       height = 6,
       units = "in")
dev.off()


#climate DBT sampling in ggplot - monthly
climate_dbt_month_sampling_plot <- ggplot(df_climate_sample2, 
                                          aes(x = climate_id, y = MDBT, 
                                              color = as.factor(climate_iter))) + 
  geom_point(shape=20, size=2) +
  xlab("Quantidade de climas") +
  ylab("Médias mensais de temperatura de bulbo seco (°C)") +
  labs(colour = "Iteração") +
  xlim(min(climate_id),max(climate_id)) + ylim(7,30) +
  #scale_y_continuous(position = "right") +
  geom_hline(yintercept = max(df_mean_sd_of_mean$MMDBT), linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = min(df_mean_sd_of_mean$MMDBT), linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position="bottom")

print(climate_dbt_month_sampling_plot)

ggsave(filename = paste0("climate_dbt_month_sampling_plot.png"),
       plot = last_plot(),
       scale = 1,
       width = 5,
       height = 6,
       units = "in")
dev.off()


#histogram of monthly values
hist(df_climate_sample2$MDBT, breaks = 10)

#clean environment
rm(a_climate, b_climate, c_climate)
rm(clima, climate_iter, climate_raw_sample)
rm(ID, MMDBT)



#### MATRIX VALUES TRANSFORMATIONS ####
#from raw sampling (0...1) to building parameter values

ls_var_mat <- list()  #matrices with building variable values

for (j in 1:number_of_methods) { #1:length(methods_vector)
  
  chosen_sample <- ls_sample_mat[[j]]
  
  #categorical and constant
  matrix_cc <- vector_cc[rep(1:nrow(vector_cc),
                             times = current_points), ]
  
  #numeric and constant
  matrix_nc <- vector_nc[rep(1:nrow(vector_nc), 
                             times = current_points), ]
  
  #categorical and variable
  matrix_cv <- matrix(nrow = hyp$final_points, ncol = number_of_cv_vars)
  
  #numeric and variable
  matrix_nv <- matrix(nrow = hyp$final_points, ncol = number_of_nv_vars)
  
  sampling_method <- methods_vector [[j]]
  
  
  #trasform numeric to discrete for categorical and variable variables
  for (cv in 1:number_of_cv_vars) {
    
    matrix_cv_equiv <- matrix_cv
    matrix_cv[ ,cv] <- chosen_sample[ ,cv]
    matrix_cv_equiv[ ,cv] <- 1/as.numeric(list_cv_levels[cv])
    matrix_cv[ ,cv] <- as.numeric(matrix_cv[ ,cv]) %/% as.numeric(matrix_cv_equiv[ ,cv])
    matrix_cv[ ,cv] <- list_cv_vars[[cv]][1+as.numeric(matrix_cv[ ,cv])]
    
  }
  
  rm(matrix_cv_equiv)
  
  #convert numeric and variable variables using ranges by a transformation function (a*x + b)
  for (nv in 1:number_of_nv_vars) {
    
    #"a" is the range (max-min); "b" is the starting value (min); "x" is the sampling value (from 0 to 1)
    matrix_nv[ ,nv] <- t(var_range[nv] %*% chosen_sample[ ,number_of_cv_vars + nv])
    matrix_nv[ ,nv] <- round(matrix_nv[ ,nv], digits = 2)
    #add each numeric matrix element by the correspondent minimum
    matrix_nv[ ,nv] <- matrix_nv[,nv] + var_min[nv]
    
  }
  
  #rename columns with variables names
  colnames(matrix_cv) <- names(list_cv_vars)
  colnames(matrix_nv) <- names(list_nv_vars)
  
  #bind all types of variables
  ls_var_mat[[j]] <- cbind(matrix_nv, matrix_nc, matrix_cv, matrix_cc)
  names(ls_var_mat)[[j]] <- methods_vector[j]
  
  #remove first 3 characters from each variable name
  colnames(ls_var_mat[[j]]) <- substring(colnames(ls_var_mat[[j]]), 4)
  
  #transpose data to new matrix and dataframe
  idf_sample <- ls_var_mat[[j]]
  idf_sample <- cbind(idf_sample, iteration_vector)
  df_idf_sample <- as.data.frame(idf_sample)
  
  
  #convert factors to numeric variables
  df_idf_sample$sur_vis_transmittance <- as.numeric(as.character(df_idf_sample$sur_vis_transmittance))
  #df_idf_sample$pilotis <- as.numeric(as.character(df_idf_sample$pilotis))
  df_idf_sample$nfloor <- as.numeric(as.character(df_idf_sample$nfloor))
  df_idf_sample$sur_nfloor <- as.numeric(as.character(df_idf_sample$sur_nfloor))
  
  #round values according to variable 
  df_idf_sample$sur_vis_transmittance <- round(df_idf_sample$sur_vis_transmittance, digits = 0)
  #df_idf_sample$pilotis <- round(df_idf_sample$pilotis, digits = 0)
  df_idf_sample$nfloor <- floor(df_idf_sample$nfloor) #opposite of ceiling function
  df_idf_sample$sur_nfloor <- floor(df_idf_sample$sur_nfloor) #opposite of ceiling function
  
  
  #write variable values in csv file that will be used for IDF creation
  write.csv(df_idf_sample,
            file = paste0("sample idf ",sampling_method,".csv"),
            row.names = FALSE)
  
}


#clean environment
rm(df.data, data, new_data)
rm(csv_data, idf_sample) #iteration_vector
rm(image.name, n.points)
rm(chosen_sample, mat_colnames)
#rm(lower_function, panel.cor, upper_function)
rm(vector_cc, list_cc_vars)
rm(vector_nc, list_nc_vars)
rm(list_nv_vars, sampling_method, cv, nv)
rm(var_min, var_max, var_range)
rm(list=ls(pattern="^matrix_"))



#### ENERGYPLUS SIMULATIONS #### run simulations by iteration number

#select desired epws (climate database)
path_epws <- "C:/Users/LabEEE_1-3/Desktop/epw_files_all/epw_files_desired"
desired_epws <- list.files(path = path_epws, pattern = ".epw")
number_of_desired_epw <- length(desired_epws)


#set working dir
setwd(path_incremental)
#list file types in incremental folder
bat_file <- list.files(pattern = ".bat$", full.names = F)
py_file <- list.files(pattern = ".py$", full.names = F)

#set iteration folder names and numeric part
iter_folder_names <- paste0("iter_",sprintf("%06d", seq(0 + iteration, hyp$n_iter + iteration)))


#create iteration folders with the necessary files
for (l in 0:hyp$n_iter) {
  
  #set working dir
  setwd(path_incremental)
  
  #delete previously copied epw files
  previous_epw_files <- list.files(pattern = ".epw$", full.names = F)
  file.remove(previous_epw_files)
  
  #old random climate sampling per iteration
  #iter_sample_epw <- sample(desired_epws, hyp$n_climates_per_iter, replace = FALSE, prob = NULL)
  
  print(paste0("Iteration number: ",iteration))
  
  #obtain iteration epw files from climate sample
  iter_sample_epw <- df_climate_sample$ID[df_climate_sample$climate_iter == iteration]
  
  #copy sampled epw files to incremental folder
  for (epw in 1:length(iter_sample_epw)) {
    
    file.copy(from = paste0(path_epws,"/",iter_sample_epw[epw]),
              to = paste0(path_incremental,"/",iter_sample_epw[epw]))
    
  }
  
  #list epw files in incremental folder
  epw_file <- list.files(pattern = ".epw$", full.names = F)
  
  
  ### SAMPLE CREATION ###
  #change working dir
  setwd(path_sample)
  
  #run python script to create IDF files
  create_idf_command <- paste ("py -2 whole_building_generator.py",l)
  system(create_idf_command)
  print("IDF files were successfully created!")
  
  #create iteration folders
  dir.create(paste0(path_incremental,"/",iter_folder_names[l+1]))
  
  #list idf files in sample folder and copy it to the iter folder
  idf_files <- list.files(pattern = ".idf$")
  
  #remove seed.idf from idf list
  idf_files <- idf_files[!(idf_files %in% "seed.idf")]
  
  #count number of files in list
  number_of_idfs <- length(idf_files)
  
  #copy each idf in the list
  for (idf in 1:number_of_idfs) {
    
    file.copy(from = paste0(path_sample,"/",idf_files[idf]),
              to = paste0(path_incremental,"/",iter_folder_names[l+1],"/",idf_files[idf]))
    
  }
  
  #delete idfs in the sample_creation folder
  file.remove(idf_files)
  
  #copy py/bat/epw/csv files to all directories with simulations
  file.copy(from = paste0(path_incremental,"/",py_file),
            to = paste0(path_incremental,"/",iter_folder_names[l+1],"/",py_file),
            overwrite = TRUE)
  
  file.copy(from = paste0(path_incremental,"/",bat_file),
            to = paste0(path_incremental,"/",iter_folder_names[l+1],"/",bat_file),
            overwrite = TRUE)
  
  file.copy(from = paste0(path_incremental,"/",epw_file),
            to = paste0(path_incremental,"/",iter_folder_names[l+1],"/",epw_file),
            overwrite = TRUE)
  
  file.copy(from = paste0(path_incremental,"/df_monthly_mean.csv"),
            to = paste0(path_incremental,"/",iter_folder_names[l+1],"/df_monthly_mean.csv"),
            overwrite = TRUE)
  
  #define iteration path
  path_iteration <- paste0(path_incremental,"/",iter_folder_names[l+1])
  
  #set working dir to iteration path
  setwd(path_iteration)
  
  #run python scripts to organize and edit simulation files
  system("py 1_edit_filename.py")
  system("py 2_filename_folder.py")
  system("py 3_change_idf_ground_temp.py")
  
  subfolders <- list.dirs(path = path_iteration, full.names = F, recursive = F)
  subfolders <- subfolders[grepl("^_",subfolders)]
  number_of_subfolders <- length(subfolders)
  
  #epws will fade out in the loop, but it is okay, because they are copied in \WeatherData\
  #copy epw files to E+\WeatherData\ directory for simulations
  for (sb in 1:number_of_subfolders) {
    
    epw_files <- list.files(pattern = ".epw", recursive = T)
    file.copy(from = paste0(path_iteration,"/",epw_files[sb]),
              to = "C:/EnergyPlusV8-9-0/WeatherData")
    
  }
  
  
  #set number of cores to run the simulations
  number_of_cores <- 10
  #parallel_simulations_per_climate <- (number_of_cores/length(subfolders))
  
  #run energyplus using batch files and cmd
  for (sb in 1:number_of_subfolders) {
    
    weather_file_name <- epw_files[sb]
    weather_file_name <- gsub(pattern = ".*/", replacement = "", weather_file_name)
    weather_file_name <- gsub(pattern = ".epw", replacement = "", weather_file_name)
    
    file.copy(from = paste0(path_iteration,"/",bat_file),
              to = paste0(path_iteration,"/",weather_file_name,"/",bat_file),
              overwrite = TRUE)
    
    setwd(paste0(path_iteration,"/",subfolders[sb]))
    simulation_command <- paste0("RunDirMulti.bat"," WeatherData\\",
                                 weather_file_name," ",
                                 number_of_cores)
    
    print(simulation_command)
    system(simulation_command, wait = TRUE, minimized = TRUE)
    
    #count number of csv files in iter/subfolder to ensure that all IDFs were simulated
    path_subfolder <- paste0(path_iteration,"/",subfolders[sb])
    number_of_csvs <- length(list.files(path_subfolder,pattern="*.csv"))
    
    while (hyp$incremental_points != number_of_csvs) {
      
      number_of_csvs <- length(list.files(path_subfolder,pattern="*.csv"))
      Sys.sleep(1)
      
    }
    
    #wait a little longer for safety
    cat("\nWait 2 seconds to ensure that the all csv files are completelly written...\n")
    Sys.sleep(2)
    cat("\nSystem ready to go, sir!\n")
    
    #remove .eso files to free some space
    eso_file <- list.files(pattern = ".eso$", full.names = F)
    file.remove(eso_file)
    
  }
  
  
  
  #set new working dir
  setwd(path_iteration)
  
  #organize monthly results in csv file
  for (sb in 1:number_of_subfolders) {
    
    #create empty list to append results dataframes
    results_df_list <- list()
    
    #list result csv files
    results_files_in_dir <- dir(path = paste0(path_incremental, "/",
                                              iter_folder_names[l+1], "/",
                                              subfolders[sb]), pattern = "*.csv")
    
    for (rs in 1:length(results_files_in_dir)) {
      
      #read csv file
      result_file <- read.csv(file = paste0(path_incremental, "/",
                                            iter_folder_names[l+1], "/",
                                            subfolders[sb], "/",
                                            results_files_in_dir[rs]))
      
      #rename colnames
      colnames(result_file) <- gsub(".IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Supply.Air.", 
                                    " ", colnames(result_file))
      
      #transpose data
      result_file <- as.data.frame(t(result_file))
      
      #rename colnames as first row
      colnames(result_file) <- as.character(unlist(result_file[1,]))
      
      #remove first row
      result_file <- result_file[-1, ]
      
      #convert factors columns to numeric 
      result_file <- factorsNumeric(result_file)
      
      #add climate, ID, zone and load type (just in case)
      result_file$Info <- row.names(result_file)
      result_file$Climate <- paste(subfolders[sb])
      result_file$ID <- results_files_in_dir[rs]
      result_file$Zone <- gsub( " .*$", "", result_file[ ,13])
      result_file$Load_type <- ifelse(grepl("Sensible", result_file$Info), "Sensible", NA)
      result_file$Load_type <- ifelse(grepl("Latent", result_file$Info), "Latent", result_file$Load_type)
      result_file$Load_type <- ifelse(grepl("Total", result_file$Info), "Total", result_file$Load_type)
      
      #remove old rownames and info column
      row.names(result_file) <- NULL
      result_file$Info <- NULL
      
      #move last columns to first
      result_file <- movetofirst(result_file, c("Climate","ID","Zone","Load_type"))
      
      #append results dataframes to list
      results_df_list[[rs]] <- result_file
      
    }
    
    city_outputs <- do.call(rbind, results_df_list)
    write.csv(city_outputs, file = paste0(path_incremental, "/",iter_folder_names[l+1], "/",
                                          subfolders[sb],"/","city_outputs.csv"), row.names = FALSE)
    
    rm(city_outputs)
    
  }
  
  
  #merge simulations from different climates
  #create empty list to append cities output dataframes
  cities_df_list <- list()
  city_outputs_files <- list.files(pattern = "city_outputs.csv", recursive = T)
  
  #print step
  print(paste0("Merging results from the following file: ", city_outputs_files))
  
  for (of in 1:length(city_outputs_files)) { 
    
    cities_df_list[[of]] <- read.csv(city_outputs_files[of])
    
  }
  
  output_iteration <- do.call(rbind, cities_df_list)
  write.csv(output_iteration, file = paste0(path_incremental, "/",
                                            iter_folder_names[l+1], "/",
                                            "iteration_outputs.csv"), row.names = FALSE)
  
  gc()
  
  
  #### SIMULATION RESULTS ####
  
  #implement "Termostato" value variations in whole_building_generator.py     DONE
  #modify HVACTemplate:Zone:IdealLoadsAirSystem object and remove dehumidification      DONE
  setwd(path_iteration)
  
  #read simulation results
  output_iteration <- read.csv("iteration_outputs.csv")
  
  #set final matrix to match the selected distribution #to match the sampling method with lower discrepancy
  #actually... sobol performs quite better than halton in preliminary ANN training, but has worse discrepancy
  
  final_matrix <- ls_var_mat[[sampling_method]] #remember to also change the method in whole_building_generator.py file
  
  #colnames(final_matrix)
  #colnames_to_round <- c("nfloor", "sur_vis_transmittance", "sur_nfloor")
  
  
  #select iteration cases from final matrix
  start_row <- 1 + l*hyp$incremental_points
  end_row <- (l + 1)*hyp$incremental_points
  iter_matrix <- final_matrix[start_row:end_row, ]
  
  #define number of thermal zones per floor: 5 is valid for core & shell office buildings
  zones_per_floor <- 5
  
  #define number of load types in simulation outputs
  number_of_load_types <- 3 #sensible, latent and total
  
  #calculate number of zones per building model
  #zones_per_building <- zones_per_floor*as.numeric(iter_matrix[ ,"nfloor"]) #if nfloor is constant
  zones_per_building <- zones_per_floor*floor(as.numeric(iter_matrix[ ,"nfloor"])) #if nfloor is variable
  
  
  #replicate inputs to match number of cases ###
  sample <- iter_matrix[rep(1:nrow(iter_matrix), times = zones_per_building), ]
  sample <- as.data.frame(sample, stringsAsFactors = FALSE)
  
  #magical data processing :)
  sample_num <- sample[ ,1:sum(number_of_nv_vars, number_of_nc_vars)]
  char_to_num <- sapply(sample_num, is.character)
  
  sample_num[char_to_num] <- lapply(sample_num[char_to_num], function(x) as.numeric(as.character(x)))
  sample_fac <- sample[ ,sum(number_of_nv_vars, number_of_nc_vars,1):length(sample[1, ])]
  
  char_to_fac <- sapply(sample_fac, is.character)
  sample_fac[char_to_fac] <- lapply(sample_fac[char_to_fac], function(x) as.factor(as.character(x)))
  
  sample2 <- cbind(sample_num, sample_fac)
  
  inputs <- sample2[rep(seq_len(nrow(sample2)), length(subfolders)), ]
  
  #replicate inputs N times (equal to number of load types) to match number of outputs
  inputs <- inputs[rep(seq_len(nrow(inputs)), each=number_of_load_types), ]
  rownames(inputs) <- NULL
  
  #merge results with the input variables
  df <- cbind(inputs,output_iteration)
  
  #trasform some variables into numeric/discrete variables again
  #df$sur_vis_transmittance <- as.numeric(as.character(df$sur_vis_transmittance))
  #df$pilotis <- as.numeric(as.character(df$pilotis)) #ativar estas linhas quando as vars forem convertidas p/ categ!!!!
  #df$nfloor <- as.numeric(as.character(df$nfloor))
  #df$sur_nfloor <- as.numeric(as.character(df$sur_nfloor))
  
  #round values according to variable  #remove round functions if variables are converterd to categorical
  df$pilotis <- round(df$pilotis, digits = 0)
  df$nfloor <- floor(df$nfloor)
  df$sur_nfloor <- floor(df$sur_nfloor)
  
  df$actual_floor <- as.numeric(as.character(sub('.*_', '', df$Zone)))
  df$delta_nfloor <- df$actual_floor - df$sur_nfloor
  
  df_file_name <- paste0("raw_df_",hyp$final_points,"_points.csv")
  write.csv(df, file = df_file_name, row.names = F)
  
  #clean enviromnent again
  rm(sample_num, sample_fac, char_to_num, char_to_fac)
  rm(epw_files, weather_file_name, df_file_name)
  
  
  #### DATA PRE-PROCESSING ####
  
  #transform output
  JtoKWh <- 3600000
  
  df$January <- round(df$January/JtoKWh, digits = 2)
  df$February <- round(df$February/JtoKWh, digits = 2)
  df$March <- round(df$March/JtoKWh, digits = 2)
  df$April <- round(df$April/JtoKWh, digits = 2)
  df$May <- round(df$May/JtoKWh, digits = 2)
  df$June <- round(df$June/JtoKWh, digits = 2)
  df$July <- round(df$July/JtoKWh, digits = 2)
  df$August <- round(df$August/JtoKWh, digits = 2)
  df$September <- round(df$September/JtoKWh, digits = 2)
  df$October <- round(df$October/JtoKWh, digits = 2)
  df$November <- round(df$November/JtoKWh, digits = 2)
  df$December <- round(df$December/JtoKWh, digits = 2)
  
  #transform IDF data to zone data
  zone_unique <- as.character(unique(df$Zone))
  df$Zone <- as.character(df$Zone)
  df$core_area <- (df$building_xlen - 2*df$shell_depth)*(df$building_xlen*df$building_ratio - 2*df$shell_depth)
  df$peri_1_3 <- (df$shell_depth*(2*df$building_xlen - 2*df$shell_depth)/2)
  df$peri_2_4 <- (df$shell_depth*(2*df$building_xlen*df$building_ratio - 2*df$shell_depth)/2)
  
  df$area <- ifelse(grepl("CORE_", df$Zone), df$core_area, NA)
  df$area <- ifelse(grepl("PZ1_", df$Zone), df$peri_1_3, df$area)
  df$area <- ifelse(grepl("PZ3_", df$Zone), df$peri_1_3, df$area)
  df$area <- ifelse(grepl("PZ2_", df$Zone), df$peri_2_4, df$area)
  df$area <- ifelse(grepl("PZ4_", df$Zone), df$peri_2_4, df$area)
  df$core_area <- df$peri_1_3 <- df$peri_2_4 <- NULL
  
  #constructions and schedule as training parameters!  #need to detail better if more values will be added !!!!!!!!!!!!
  #df$floor_construction <- ifelse(grepl("PisoCeramica", df$floor_construction), 1, 0)
  df$glass_construction <- ifelse(grepl("Vid2", df$glass_construction), 0.82, 0.38)
  df$sur_glass <- ifelse(grepl("Vid2", df$sur_glass), 0.82, 0.38)
  df$schedule <- as.numeric(substring(df$schedule, 5, 6))
  
  #as que começam com 1 são leves... as que começam com 3 são pesadas
  df$ct_extwall <- ifelse(grepl("Parede11", df$extwall_construction), 1, NA)
  df$ct_extwall <- ifelse(grepl("Parede12", df$extwall_construction), 1, df$ct_extwall)
  df$ct_extwall <- ifelse(grepl("Parede13", df$extwall_construction), 1, df$ct_extwall)
  df$ct_extwall <- ifelse(grepl("Parede21", df$extwall_construction), 2, df$ct_extwall)
  df$ct_extwall <- ifelse(grepl("Parede22", df$extwall_construction), 2, df$ct_extwall)
  df$ct_extwall <- ifelse(grepl("Parede23", df$extwall_construction), 2, df$ct_extwall)
  df$ct_extwall <- ifelse(grepl("Parede31", df$extwall_construction), 3, df$ct_extwall)
  df$ct_extwall <- ifelse(grepl("Parede32", df$extwall_construction), 3, df$ct_extwall)
  df$ct_extwall <- ifelse(grepl("Parede33", df$extwall_construction), 3, df$ct_extwall)
  
  #as que terminam com 1 são isolantes... as que terminam com 3 são condutoras
  df$u_extwall <- ifelse(grepl("Parede11", df$extwall_construction), 1, NA)
  df$u_extwall <- ifelse(grepl("Parede12", df$extwall_construction), 2, df$u_extwall)
  df$u_extwall <- ifelse(grepl("Parede13", df$extwall_construction), 3, df$u_extwall)
  df$u_extwall <- ifelse(grepl("Parede21", df$extwall_construction), 1, df$u_extwall)
  df$u_extwall <- ifelse(grepl("Parede22", df$extwall_construction), 2, df$u_extwall)
  df$u_extwall <- ifelse(grepl("Parede23", df$extwall_construction), 3, df$u_extwall)
  df$u_extwall <- ifelse(grepl("Parede31", df$extwall_construction), 1, df$u_extwall)
  df$u_extwall <- ifelse(grepl("Parede32", df$extwall_construction), 2, df$u_extwall)
  df$u_extwall <- ifelse(grepl("Parede33", df$extwall_construction), 3, df$u_extwall)
  df$extwall_construction <- NULL
  
  #as que começam com 1 são leves... as que começam com 3 são pesadas
  df$ct_roof <- ifelse(grepl("Cobertura11", df$roof_construction), 1, NA)
  df$ct_roof <- ifelse(grepl("Cobertura12", df$roof_construction), 1, df$ct_roof)
  df$ct_roof <- ifelse(grepl("Cobertura13", df$roof_construction), 1, df$ct_roof)
  df$ct_roof <- ifelse(grepl("Cobertura21", df$roof_construction), 2, df$ct_roof)
  df$ct_roof <- ifelse(grepl("Cobertura22", df$roof_construction), 2, df$ct_roof)
  df$ct_roof <- ifelse(grepl("Cobertura23", df$roof_construction), 2, df$ct_roof)
  df$ct_roof <- ifelse(grepl("Cobertura31", df$roof_construction), 3, df$ct_roof)
  df$ct_roof <- ifelse(grepl("Cobertura32", df$roof_construction), 3, df$ct_roof)
  df$ct_roof <- ifelse(grepl("Cobertura33", df$roof_construction), 3, df$ct_roof)
  
  #as que terminam com 1 são isolantes... as que terminam com 3 são condutoras
  df$u_roof <- ifelse(grepl("Cobertura11", df$roof_construction), 1, NA)
  df$u_roof <- ifelse(grepl("Cobertura12", df$roof_construction), 2, df$u_roof)
  df$u_roof <- ifelse(grepl("Cobertura13", df$roof_construction), 3, df$u_roof)
  df$u_roof <- ifelse(grepl("Cobertura21", df$roof_construction), 1, df$u_roof)
  df$u_roof <- ifelse(grepl("Cobertura22", df$roof_construction), 2, df$u_roof)
  df$u_roof <- ifelse(grepl("Cobertura23", df$roof_construction), 3, df$u_roof)
  df$u_roof <- ifelse(grepl("Cobertura31", df$roof_construction), 1, df$u_roof)
  df$u_roof <- ifelse(grepl("Cobertura32", df$roof_construction), 2, df$u_roof)
  df$u_roof <- ifelse(grepl("Cobertura33", df$roof_construction), 3, df$u_roof)
  df$roof_construction <- NULL
  
  
  #could be useful to add facade length as a training parameter :)
  #to avoid high frequency of zero values (core), I calculated the zone mean "internal" length value
  df$zone_facade_length <- ifelse(grepl("CORE_", df$Zone), 0, NA)
  #facade length for the perimetral zones exposed facade
  df$zone_facade_length <- ifelse(grepl("PZ1_", df$Zone), df$building_xlen, df$zone_facade_length)
  df$zone_facade_length <- ifelse(grepl("PZ2_", df$Zone), df$building_ratio*df$building_xlen, df$zone_facade_length)
  df$zone_facade_length <- ifelse(grepl("PZ3_", df$Zone), df$building_xlen, df$zone_facade_length)
  df$zone_facade_length <- ifelse(grepl("PZ4_", df$Zone), df$building_ratio*df$building_xlen, df$zone_facade_length)
  #round length values
  df$zone_facade_length <- round(df$zone_facade_length, digits = 2)
  
  #ground and roof exposure (create N possible combinations for exposure)
  df$pilotis <- ifelse(endsWith(df$Zone,"_1"), df$pilotis, 0)
  df$ground_exp <- ifelse(endsWith(df$Zone,"_1"), 1, -1)
  df$ground_exp <- ifelse(df$pilotis == 1, -1, df$ground_exp)
  #df$adiab #not needed :)
  
  #figure out a way to describe roof exposure and the absortance
  df$roof_exp <- ifelse(endsWith(df$Zone,paste0("_",df$nfloor)), 1, -1)
  df$abs_roof_construction <- ifelse(endsWith(df$Zone,paste0("_",df$nfloor)), df$abs_roof_construction, 0)
  
  #surroundings corrections, caution with possible correlations in dataframe!!!!!
  
  #surroundings transmittance could be only 0 or 1
  df$sur_vis_transmittance <- round(df$sur_vis_transmittance, digits = 0)
  
  #horizontal density tends to zero when surroundings visual transmittance is 100%
  df$horizontal_building_density <- ifelse(df$sur_vis_transmittance == 0, df$horizontal_building_density, 0)
  df$sur_abs_extwall <- ifelse(df$sur_vis_transmittance == 0, df$sur_abs_extwall, 0)
  df$sur_wwr <- ifelse(df$sur_vis_transmittance == 0, df$sur_wwr, 0)
  
  
  #orientation
  df$north_south <- ifelse(grepl("PZ1_", df$Zone), 1, NA) #faces north
  df$north_south <- ifelse(grepl("PZ3_", df$Zone), -1, df$north_south) #faces south
  df$north_south <- ifelse(grepl("PZ2_", df$Zone), 0, df$north_south) #faces west
  df$north_south <- ifelse(grepl("PZ4_", df$Zone), 0, df$north_south) #faces east
  df$north_south <- ifelse(grepl("CORE_", df$Zone), 0, df$north_south)
  
  df$east_west <- ifelse(grepl("PZ2_", df$Zone), -1, NA) #faces west
  df$east_west <- ifelse(grepl("PZ4_", df$Zone), 1, df$east_west) #faces east
  df$east_west <- ifelse(grepl("PZ1_", df$Zone), 0, df$east_west) #faces north
  df$east_west <- ifelse(grepl("PZ3_", df$Zone), 0, df$east_west) #faces south
  df$east_west <- ifelse(grepl("CORE_", df$Zone), 0, df$east_west)
  
  # df$zone_orientation <- ifelse(grepl("CORE_", df$Zone), -180, NA) # "core"
  # df$zone_orientation <- ifelse(grepl("PZ1_", df$Zone), 0+df$azimuth, df$zone_orientation)
  # df$zone_orientation <- ifelse(grepl("PZ2_", df$Zone), 270+df$azimuth, df$zone_orientation)
  # df$zone_orientation <- ifelse(grepl("PZ3_", df$Zone), 180+df$azimuth, df$zone_orientation)
  # df$zone_orientation <- ifelse(grepl("PZ4_", df$Zone), 90+df$azimuth, df$zone_orientation)
  
  #new adjustments for thermal zones
  df$abs_extwall_construction <- ifelse(grepl("CORE_", df$Zone), 0, df$abs_extwall_construction)
  df$horizontal_building_density <- ifelse(grepl("CORE_", df$Zone), 0.5, df$horizontal_building_density) #use 0.5 because it is the interval end that emulate very close buildings
  df$shading <- ifelse(grepl("CORE_", df$Zone), 0, df$shading)
  df$sur_abs_extwall <- ifelse(grepl("CORE_", df$Zone), 0, df$sur_abs_extwall)
  df$sur_wwr <- ifelse(grepl("CORE_", df$Zone), 0, df$sur_wwr)
  df$wwr <- ifelse(grepl("CORE_", df$Zone), 0, df$wwr)
  df$glass_construction <- ifelse(grepl("CORE_", df$Zone), 0, df$glass_construction)
  df$delta_nfloor <- ifelse(grepl("CORE_", df$Zone), 0, df$delta_nfloor)
  df$ct_extwall <- ifelse(grepl("CORE_", df$Zone), 0, df$ct_extwall)
  df$u_extwall <- ifelse(grepl("CORE_", df$Zone), 0, df$u_extwall)
  #zone facade wass alrealdy adjusted
  
  
  #thermal load per area
  df$January_m2 <- round(df$January/df$area, digits = 2)
  df$February_m2 <- round(df$February/df$area, digits = 2)
  df$March_m2 <- round(df$March/df$area, digits = 2)
  df$April_m2 <- round(df$April/df$area, digits = 2)
  df$May_m2 <- round(df$May/df$area, digits = 2)
  df$June_m2 <- round(df$June/df$area, digits = 2)
  df$July_m2 <- round(df$July/df$area, digits = 2)
  df$August_m2 <- round(df$August/df$area, digits = 2)
  df$September_m2 <- round(df$September/df$area, digits = 2)
  df$October_m2 <- round(df$October/df$area, digits = 2)
  df$November_m2 <- round(df$November/df$area, digits = 2)
  df$December_m2 <- round(df$December/df$area, digits = 2)
  
  df$January <- df$February <- df$March <- df$April <- df$May <- df$June <- NULL
  df$July <- df$August <- df$September <- df$October <- df$November <- df$December <- NULL
  
  #move some columns to first
  df <- movetofirst(df, c("Climate","ID","Zone","Load_type"))
  
  #cut apart monthly cooling load values
  df_load <- df[ ,(length(colnames(df))-11):length(colnames(df))]
  
  #transpose monthly columns to a single vector
  cooling_loads <- c(t(df_load))
  
  #repeat 12x each row of dataframe
  df2 <- df[ ,1:(length(colnames(df))-12)]
  df2 <- df2[rep(seq_len(nrow(df2)), each=12), ]
  row.names(df2) <- NULL
  
  #add month number to dataframe
  df2$Month <- sprintf("%02d", rep(1:12)) #add zeros before number
  
  
  #reunite dataframes
  df3 <- cbind(df2, cooling_loads)
  row.names(df3) <- NULL
  df3$Climate <- gsub("[_]", " ", df3$Climate) #underline replaced by whitespace
  df3$Climate <- gsub("[.]", " ", df3$Climate) #dot replaced by whitespace
  
  df3$Climate <- trimws(df3$Climate, "l") #trim/remove leading whitespace
  df3$ID_Month <- paste(df3$Climate, df3$Month)
  
  df3$Month <- NULL
  df3 <- movetofirst(df3, c("ID_Month","Climate","ID","Zone","Load_type"))
  
  #load mean values of climatic variables
  df_monthly_mean_backup <- read.csv("df_monthly_mean.csv")
  
  #maintain only relevant info and climatic variables: dbt, dpt, ru, ap, ghr
  df_monthly_mean <- df_monthly_mean_backup[,c(1:3, 8:12)]
  
  #rename dataframe colnames
  colnames(df_monthly_mean) <- c("ID_Month", "City", "State",
                                 "Monthly_mean_DBT", "Monthly_mean_DPT", "Monthly_mean_RH", 
                                 "Monthly_mean_AP", "Monthly_mean_GHR")
  
  #round climatic variables
  df_monthly_mean$Monthly_mean_DBT <- round(df_monthly_mean$Monthly_mean_DBT, 1)
  df_monthly_mean$Monthly_mean_DPT <- round(df_monthly_mean$Monthly_mean_DPT, 1)
  df_monthly_mean$Monthly_mean_AP <- round(df_monthly_mean$Monthly_mean_AP, 1)
  df_monthly_mean$Monthly_mean_RH <- round(df_monthly_mean$Monthly_mean_RH, 1)
  df_monthly_mean$Monthly_mean_GHR <- round(df_monthly_mean$Monthly_mean_GHR, 1)
  
  
  #add month number to this dataframe as well
  df_monthly_mean$Month <- sprintf("%02d", rep(1:12))
  df_monthly_mean$ID_Month <- 0
  df_monthly_mean$ID_Month <- paste(df_monthly_mean$State, df_monthly_mean$City, df_monthly_mean$Month)
  
  df4 <- merge(df3, df_monthly_mean, by = "ID_Month")
  
  
  #bind dataframes in one
  ifelse(l == 0, all_dfs <- df4, all_dfs <- rbind(all_dfs,df4))
  write.csv(all_dfs, file = "df_io.csv", row.names = F)
  
  
  
  
  #start from here if "continue_iter == TRUE" !!!!!!!!! MAKE IT WORK 1 !!!!!!!!!!!!!!!!!!!!!!!
  #all_dfs <- read.csv("df_io.csv")
  
  
  #Zero- and Near Zero-Variance Predictors
  #useful to identify nzv variables
  nzv <- nearZeroVar(all_dfs, freqCut = 99/1,
                     uniqueCut = 10, saveMetrics= TRUE,
                     foreach = TRUE, allowParallel = FALSE)
  print(nzv)
  #simplified list to remove those nzv variables
  nzv_list <- nearZeroVar(all_dfs, freqCut = 99/1,
                          uniqueCut = 10, saveMetrics= FALSE,
                          foreach = TRUE, allowParallel = FALSE)
  print(nzv_list)
  filtered_df <- all_dfs[, -nzv_list]
  
  #remove extra info
  filtered_df$ID_Month <- filtered_df$Climate <- filtered_df$ID <-  filtered_df$Zone <-  NULL
  filtered_df$area <- filtered_df$building_ratio <- filtered_df$building_xlen <- NULL
  filtered_df$zone_facade_length <- NULL
  
  filtered_df$City <- filtered_df$State <- filtered_df$Month <- NULL
  
  
  #transformar as variaveis para o treinamento das redes neurais
  #arranjar um jeito de descrever as variaveis do entorno: o que importa ? o angulo (building ratio + grid urbano) 
  #considerar elaborar um metamodelo para zonas internas e outro para zonas externas!!! facilitaria as descricoes!!!
  
  #choose just the wanted output
  filtered_df <- filtered_df[filtered_df$Load_type == "Total", ]
  
  #remove output type
  filtered_df$Load_type <- NULL
  
  #remove unnecessary climatic variables (use DBT for sensible load and DPT for latent load)
  filtered_df$Monthly_mean_RH <- NULL
  filtered_df$Monthly_mean_AP <- NULL
  #filtered_df$Monthly_mean_GHR <- NULL
  
  #print column names
  print(colnames(filtered_df))
  
  
  #add code to remove highly correlated variables!!!!!!!!!!!!!!!!! manual remove for now...
  #filtered_df$sur_abs_extwall <- NULL
  #filtered_df$horizontal_building_density <- NULL
  
  filtered_df$nfloor <- NULL
  filtered_df$sur_nfloor <- NULL
  filtered_df$actual_floor <- NULL
  
  
  ### Pre-training data check
  
  # #generate an exploratory histogram
  # png(filename="hist_df.png", width = 900, height = 900, units = "px")
  # hist.data.frame(filtered_df, n.unique = 2, nclass = 35)
  # dev.off()
  
  #renaming variables for correlograms plot
  filtered_df_renamed <- filtered_df
  colnames(filtered_df_renamed)
  
  renome <- c("Absortância parede", "Absortância cobertura", "Pé direito", "Densidade equipamentos",
              "Densidade urbana horizontal", "Densidade iluminação", "Densidade pessoas",
              "Componente sombreamento", "Absortância entorno opaco", "WWR entorno",
              "Termostato", "WWR", "Fator solar vidro", "Padrão de ocupação", "Delta pavimentos", 
              "CT parede", "U parede", "CT cobertura", "U cobertura", "Exposição piso", "Exposição cobertura", 
              "Norte Sul", "Leste Oeste", "Carga térmica refrigeração",
              "Temperatura bulbo seco", "Temperatura ponto orvalho", "Irradiação global horizontal")
  
  print(renome)
  
  colnames(filtered_df_renamed) <- renome
  
  #relocate output to last column
  filtered_df_renamed <- filtered_df_renamed %>% select(-"Carga térmica refrigeração","Carga térmica refrigeração")

  #correlation matrix
  cor_matrix <- cor(filtered_df_renamed)
  round(cor_matrix, 2)
  
  #correlograms 
  png(filename="corr_plot_matrix.png", width = 900, height = 900, units = "px")
  corrplot(cor_matrix, type = "lower", order = "original", tl.col = "black", tl.srt = 0.001)
  dev.off()
  
  png(filename="corr_plot_mixed.png", width = 900, height = 900, units = "px")
  corrplot.mixed(cor_matrix, lower = "pie", upper = "number", tl.pos = "lt", 
                 order = "original", tl.col = "black", tl.srt = 40, number.cex = 0.75, bg = "black")
  dev.off()
  gc()
  
  #ANIMATE CORRELOGRAMS to show the sample evolution balance (more samples = less correlation)!!!!!!!!!
  #filter highly correlated input variables
  #highlyCorDescr <- findCorrelation(cor_matrix, cutoff = .75)  #copy until here to test_dataset.r
  
  #until here it has to be identical to test_dataset.r (except for the paths)
  
  
  #### ANN TRAINING ####
  
  #backup
  filtered_df_backup <- filtered_df
  
  #reproducible results
  set.seed(generator)
  
  #randomize/shuffle the data (rows) before training
  filtered_df <- filtered_df[sample(nrow(filtered_df)), ]
  
  #reduce detail in dataframe values
  #filtered_df <- round(filtered_df, digits = 2)
  write.csv(filtered_df, file = "filtered_df.csv", row.names = F)
  
  #old way - creates balanced splits of the data -
  inTraining <- createDataPartition(filtered_df$cooling_loads, p = .80, list = FALSE)

  training <- filtered_df[ inTraining,]
  write.csv(training, file = "training_df.csv", row.names = F)

  validation <- filtered_df[ -inTraining,]
  write.csv(validation, file = "validation_df.csv", row.names = F)
  
  
  # #alternative way (training = 100% sample, and validation is large dataset previously simulated)
  # training <- filtered_df
  # #read external validation dataset (much larger)
  # validation <- read.csv(file = "C:\\Users\\LabEEE_1-3\\Desktop\\test_dataset\\iter_000004\\filtered_df.csv")
  # # #randomize/shuffle the data (rows)
  # # validation <- validation[sample(nrow(validation)), ]
  
  
  
  
  #load files
  #training <- read.csv(file = "C:\\Users\\LabEEE_1-3\\Desktop\\incremental\\iter_000009\\training_df.csv")
  #validation <- read.csv(file = "C:\\Users\\LabEEE_1-3\\Desktop\\incremental\\iter_000009\\validation_df.csv")
  
  #training <- read.csv(file = "/home/leo/Documents/gpu_test/training_df.csv")
  #validation <- read.csv(file = "/home/leo/Documents/gpu_test/validation_df.csv")
  
  #training$Monthly_mean_GHR <- NULL # pessima ideia... testei e GHR ajuda bastante na predicao
  #validation$Monthly_mean_GHR <- NULL # pessima ideia... testei e GHR ajuda bastante na predicao
  
  #use just PERIMETRAL results
  #training <- training[!(training$north_south == 0 & training$east_west == 0), ]
  #validation <- validation[!(validation$north_south == 0 & validation$east_west == 0), ]
  

  
  # #feature plot
  # featurePlot(x = validation[,-ncol(validation)],
  #             y = validation$cooling_loads,
  #             plot = "scatter")
  # #hist.data.frame(training, n.unique = 2, nclass = 20)
  
  #seen ann_inputs
  ann_inputs <- data.matrix(subset(training, select= -c(cooling_loads)))
  #print ann_inputs matrix dimension
  print(paste("ANN inputs matrix dimension is:", dim(ann_inputs)[1],"x", dim(ann_inputs)[2]))
  
  #normalize seen ann_inputs
  ann_inputs_center_scale <- scale(ann_inputs, center = T, scale = T) 
  #write.csv(ann_inputs_center_scale, file = "norm_ann_inputs.csv", row.names = F)
  
  #output from training cases
  output <- training$cooling_loads

  #possible output transformation to normalize outputs  
  hist(output, breaks = 100)
  #hist(output^0.5, breaks = 100) #square root
  #hist(output^0.25, breaks = 100) #quadratic root

  #log10 transformation: increment allows to create a range of results from 0 to almost +2
  #increm <- 1 #0.1:NRMSE=2.5%, 1:NRMSE=2%
  #output <- log10(output + increm) 
  #hist(output, breaks = 100)
  

  
  #unseen ann_inputs
  unseen_ann_inputs <- data.matrix(subset(validation, select= -c(cooling_loads)))
  #print unseen ann_inputs matrix dimension
  print(paste("ANN unseen inputs matrix dimension is:", dim(unseen_ann_inputs)[1],"x", dim(unseen_ann_inputs)[2]))
  
  #normalize unseen ann_inputs
  unseen_ann_inputs_center_scale <- scale(unseen_ann_inputs, center = T, scale = T) 
  #write.csv(unseen_ann_inputs_center_scale, file = "unseen_norm_ann_inputs.csv", row.names = F)
  
  #output from unseen cases
  unseen_output <- validation$cooling_loads
  
  #possible output transformation to normalize outputs  
  hist(unseen_output, breaks = 100)
  #hist(unseen_output^0.5, breaks = 100) #square root
  #hist(unseen_output^0.25, breaks = 100) #quadratic root
  
  #log10 transformation: 0.01 allows to create a range of results from 0 to almost +2
  #unseen_output <- log10(unseen_output + increm) 
  #hist(unseen_output, breaks = 100)
  
  
  print(paste("Seen output count number is:",length(output)))
  print(paste("Unseen output count number is:",length(unseen_output)))
  
  
  
  #### DEEP NEURAL NETWORK ####
  
  ### MXNET Simbolyc Mode ###

  net <- mx.symbol.Variable("data") %>%
    # drop out a % of predictors
    #mx.symbol.Dropout(p=0.00, name='Predictor_Dropout') %>%

    # a fully connected layer with N units
    mx.symbol.FullyConnected(num_hidden=32, name='fc_1') %>% #1st hidden layer
    # batch normalize the units
    #mx.symbol.BatchNorm(name='bn_1') %>%                                               #considering it caused the validation rmse to be lower than training rmse
    # use the rectified linear unit (relu) for the activation function
    mx.symbol.Activation(act_type='relu', name='relu_1') %>%
    # drop out a % of
    #mx.symbol.Dropout(p=0.05, name='dropout_1') %>%

    # a fully connected layer with N units
    mx.symbol.FullyConnected(num_hidden=16, name='fc_2') %>% #2st hidden layer
    # batch normalize the units
    #mx.symbol.BatchNorm(name='bn_2') %>%
    # use the rectified linear unit (relu) for the activation function
    mx.symbol.Activation(act_type='relu', name='relu_2') %>%
    # drop out a % of
    #mx.symbol.Dropout(p=0.05, name='dropout_2') %>%

    # # a fully connected layer with N units
    # mx.symbol.FullyConnected(num_hidden=2, name='fc_3') %>% #3st hidden layer
    # # batch normalize the units
    # #mx.symbol.BatchNorm(name='bn_3') %>%
    # # use the rectified linear unit (relu) for the activation function
    # mx.symbol.Activation(act_type='relu', name='relu_3') %>%
    # # drop out a % of
    # #mx.symbol.Dropout(p=0.05, name='dropout_3') %>%
    #
    # # a fully connected layer with N units
    # mx.symbol.FullyConnected(num_hidden=2, name='fc_4') %>% #4st hidden layer
    # # batch normalize the units
    # #mx.symbol.BatchNorm(name='bn_4') %>%
    # # use the rectified linear unit (relu) for the activation function
    # mx.symbol.Activation(act_type='relu', name='relu_4') %>%
    # # drop out a % of
    # #mx.symbol.Dropout(p=0.00, name='dropout_4') %>%
    #
    # # a fully connected layer with N units
    # mx.symbol.FullyConnected(num_hidden=2, name='fc_5') %>% #4st hidden layer
    # # batch normalize the units
    # #mx.symbol.BatchNorm(name='bn_5') %>%
    # # use the rectified linear unit (relu) for the activation function
    # mx.symbol.Activation(act_type='relu', name='relu_5') %>%
    # # drop out a % of
    # #mx.symbol.Dropout(p=0.00, name='dropout_5') %>%

    #a fully connected layer with 1 unit
    mx.symbol.FullyConnected(num_hidden=1, name='out') %>% #edit here to add other outputs
    #use the linear regression output
    mx.symbol.LinearRegressionOutput(name='output')

  arguments(net)

  #graph.viz(net)
  graph.viz(net,
    shape = NULL, direction = "LR", type = "graph",
    graph.width.px = NULL, graph.height.px = NULL)


  #set number of epochs and batch size
  epochs <- 1000
  batch_size <- 32 ##batch size must be equal or lower to evaluation data rows #ideally lower than 32 and a multiple of 2 (memory optimum)
  #learning_rate <- 0.001
  logger_interval <- length(filtered_df/batch_size)


  #if this is the first iteration
  if (iteration == 0) {

    # save training and evaluation errors for later use
    logger <- mx.metric.logger$new()

    # set the random seed
    mx.set.seed(generator)

    # train the model
    model <- mx.model.FeedForward.create(
      symbol            = net,    # symbolic network
      X                 = ann_inputs_center_scale, # predictors
      y                 = output, # the response
      optimizer         = "adam", # use "adam" (Adam optimization method) | "sgd" (Stochastic gradient descent) | "adagrad"
      eval.data         = list(data=unseen_ann_inputs_center_scale,
                               label=unseen_output), # change eval data for next iter simulations !!!!!!!!

      ctx               = mx.cpu(), # use the gpu for training
      eval.metric       = mx.metric.rmse, # evaluate with rmse
      num.round         = epochs,     # number of epochs
      #learning.rate     = learning_rate,   # learning rate is already set by the optimizer argument above
      #momentum          = 0.9, # works only with "sgd" (Stochastic gradient descent) optimizer
      #initializer=mx.init.uniform(0.07), #0.01

      epoch.end.callback = mx.callback.early.stop(train.metric = 0.8, eval.metric = 1,
                                                  bad.steps = 10, maximize = F, verbose = T),
      batch.end.callback = mx.callback.log.train.metric(logger_interval, logger),

      array.batch.size  = batch_size,    # batch size
      array.layout      = "rowmajor"  # the data is stored in row major format
    )

    #save model checkpoint into file
    mx.model.save(model, prefix = paste0("ann_iter_",iteration),
                  iteration = epochs)

    #save logger info
    png(filename=paste0("logger_eval_",iteration,".png"), width = 800, height = 600, units = "px")
    plot(logger$eval, col="red", xlab="Número de batches", ylab="NRMSE (%)", ylim=c(0,10))
    dev.off()

    logger_eval <- logger$eval
    write.csv(logger_eval, file = "logger_eval.csv", row.names = T)


    png(filename=paste0("logger_train_",iteration,".png"), width = 800, height = 600, units = "px")
    plot(logger$train, col="blue", xlab="Número de batches", ylab="NRMSE (%)", ylim=c(0,10))
    dev.off()

    logger_train <- logger$train
    write.csv(logger_train, file = "logger_train.csv", row.names = T)

  }

  #if it is not the first iteration and transfer learning is wanted
  if (iteration != 0 & transfer_learning == TRUE) {

    # save training and evaluation errors for later use
    logger <- mx.metric.logger$new()

    # set the random seed
    mx.set.seed(generator)

    ### Transfer Learning ###
    #Load model checkpoint from file
    model_loaded <- mx.model.load(prefix = paste0(path_incremental, "/",iter_folder_names[iteration],"/","ann_iter_",iteration-1),
                                  iteration = epochs)

    #continue training from loaded model with the next iteration data
    model <- mx.model.FeedForward.create(
       #imported parameters from previous model
       symbol = model_loaded$symbol,
       arg.params = model_loaded$arg.params,
       aux.params = model_loaded$aux.params,

       #ideally same parameters as previous model
       X = ann_inputs_center_scale, #the predictors from new iteration
       y = output, # the response from new iteration
       optimizer         = "adam", # use "adam" (Adam optimization method) or "sgd" (Stochastic gradient descent)
       eval.data         = list(data=unseen_ann_inputs_center_scale,
                                label=unseen_output), # change eval data for next iter simulations !!!!!!!!

       ctx               = mx.cpu(), # use the gpu for training
       eval.metric       = mx.metric.rmse, # evaluate with rmse
       num.round         = epochs,     # number of epochs
       #learning.rate     = learning_rate,   # learning rate is already set by the optimizer argument above
       #momentum          = 0.9, # works only with "sgd" (Stochastic gradient descent) optimizer
       #initializer=mx.init.uniform(0.07), #0.01

       epoch.end.callback = mx.callback.early.stop(train.metric = 0.8, eval.metric = 1,
                                                   bad.steps = 10, maximize = F, verbose = T),
       batch.end.callback = mx.callback.log.train.metric(logger_interval, logger),

       array.batch.size  = batch_size,    # batch size
       array.layout      = "rowmajor"  # the data is stored in row major format
     )

    #save model checkpoint into file
    mx.model.save(model, prefix = paste0("ann_iter_",iteration),
                  iteration = epochs)

    #save logger info
    png(filename=paste0("logger_eval_",iteration,".png"), width = 800, height = 600, units = "px")
    plot(logger$eval, col="red", xlab="Número de batches", ylab="NRMSE (%)", ylim=c(0,10))
    dev.off()

    logger_eval <- logger$eval
    write.csv(logger_eval, file = "logger_eval.csv", row.names = T)


    png(filename=paste0("logger_train_",iteration,".png"), width = 800, height = 600, units = "px")
    plot(logger$train, col="blue", xlab="Número de batches", ylab="NRMSE (%)", ylim=c(0,10))
    dev.off()

    logger_train <- logger$train
    write.csv(logger_train, file = "logger_train.csv", row.names = T)

  }
  
  
  
  #unserialize MXNet model from R object
  #model_export <- mx.unserialize(model)
  
  
  #montar o hyperparameters grid e a multi-threaded distributed parallel computation
  #(talvez valha a pena verificar o pacote H2O)!!!!!!!
  
  # early stop with different conditions # more info at mxnet-r-reference-manual.pdf
  # mx.callback.early.stop(train.metric = NULL, eval.metric = NULL, 
  #                        bad.steps = NULL, maximize = FALSE, verbose = FALSE)
  
  # log training metric each period
  #mx.callback.log.train.metric(period, logger = NULL)
  
  
  #### BASIC MXNET ####
  # mx.set.seed(generator)
  # model <- mx.mlp(ann_inputs_center_scale, output,
  #                 hidden_node=c(50,20), out_node=1, out_activation="rmse",
  #                 num.round=500, array.batch.size=batch_size, 
  #                 learning.rate=1e-3, momentum=0.9,
  #                 eval.metric=mx.metric.rmse)
  
  # ### MXNET pure ###
  # 
  # #use N CPU threads ??? #does not seem to work
  # #Sys.setenv('MXNET_CPU_WORKER_NTHREADS'=10)
  # 
  # # set the random seed
  # mx.set.seed(generator)
  # 
  # data <- mx.symbol.Variable("data")
  # fc1 <- mx.symbol.FullyConnected(data=data, name="fc1", num_hidden=30)  #1st hidden layer
  # act1 <- mx.symbol.Activation(data=fc1, name="relu1", act_type="relu")
  # fc2 <- mx.symbol.FullyConnected(data=act1, name="fc2", num_hidden=20)  #2st hidden layer
  # act2 <- mx.symbol.Activation(data=fc2, name="relu2", act_type="relu")
  # fc3 <- mx.symbol.FullyConnected(data=act2, name="fc3", num_hidden=1) #change num_hidden if there are more output vars
  # lro <- mx.symbol.LinearRegressionOutput(data=fc3, name="lro1")
  # 
  # #train deep neural network
  # mx.set.seed(generator)
  # model <- mx.model.FeedForward.create(symbol = lro,
  #                                      X = ann_inputs_center_scale, y = output,
  #                                      ctx = mx.cpu(), #mx.cpu/mx.gpu
  #                                      begin.round = 1,
  #                                      num.round = epochs, # number of epochs
  #                                      optimizer = "adam",
  #                                      initializer = mx.init.uniform(0.01),
  #                                      learning.rate = 0.001,
  #                                      #momentum = 0.8, #just use it in optimizer = "sgd"
  #                                      eval.metric = mx.metric.rmse, #mx.metric.mse
  #                                      array.batch.size = batch_size, #batch size
  #                                      array.layout = "rowmajor",
  #                                      metric_cpu = TRUE,
  #                                      verbose = TRUE
  # )
  # 
  # #Save model checkpoint into file
  # mx.model.save(model, prefix = paste0("ann_iter_",l), iteration = epochs)
  # 
  # #Plot computation graph
  # graph.viz(model$symbol)
  
  
  #### MXNET via caret #####
  # # A basic script using caret to interface with mxnet
  # 
  # # CREATE CLUSTER FOR FASTER PROCESSING
  # ## LINUX
  # # registerDoMC(cores=10)
  # ## WINDOWS - the following line will create a local N-node snow cluster
  # workers <- makeCluster(10, type="SOCK")
  # registerDoParallel(workers)
  # 
  # #reproducible results
  # set.seed(generator)
  # 
  # #inform dataframe dimensions
  # cat(sprintf("Training set has %d rows and %d columns\n", nrow(ann_inputs), ncol(ann_inputs)))
  # 
  # # #grid for mxnet method
  # # mxnet.params <- expand.grid(layer1 = c(40,50),
  # #                             layer2 = c(20,30),
  # #                             layer3 = 0,
  # #                             learning.rate = 1e-4, #c(1e-05,1e-04,1e-03),
  # #                             momentum = 0.9, #c(0.5,0.7,0.9),
  # #                             dropout = 0, #c(0.1,0.2,0.3),
  # #                             activation = 'relu' #c('relu', 'sigmoid', 'tanh', 'softrelu') #'relu'
  # # )
  # 
  # #grid for mxnetAdam method
  # mxnetAdam.params <- expand.grid(layer1 = 64, #c(32,64),
  #                             layer2 = 64,
  #                             layer3 = 64, #c(32,64),
  #                             beta1 = 0.6, 
  #                             beta2 = 0.9999,
  #                             dropout = 0.1, #c(0,0.2), #seq(0, .7, length = len),
  #                             learningrate = 2e-3, #c(2e-4,3e-4),
  #                             activation = c('relu','softrelu') #c('relu', 'sigmoid', 'tanh', 'softrelu') #'relu'
  # )
  # 
  # #only fits one model to the entire training set
  # #ctrl.mxnet <- trainControl(method="none")
  # 
  # #use one of the following resampling controls in case of a hyperparameter grid
  # #cross-validation
  # ctrl.mxnet <- trainControl(method = "cv", number = 10,
  #                            returnResamp = "all",
  #                            search = "random",
  #                            allowParallel = TRUE,
  #                            verboseIter = TRUE
  #                            )
  # 
  # #repeated cross-validation
  # # ctrl.mxnet <- trainControl(method="repeatedcv", number = 5, repeats = 4,
  # #                            p = 0.95, #0.75
  # #                            search = "grid", verboseIter = TRUE,
  # #                            trim = FALSE, allowParallel = TRUE)
  # 
  # fitControl <- trainControl(
  #   method = "adaptive_cv",
  #   number = 5,  repeats = 4,               # Crossvalidation (20 Folds will be created)
  #   adaptive = list(min =3,                 # minimum number of resamples per hyperparameter
  #                   alpha =0.05,            # Confidence level for removing hyperparameters
  #                   method = "BT",          # Bradly-Terry Resampling method (here you can instead also use "gls")
  #                   complete = FALSE),      # If TRUE a full resampling set will be generated
  #   search = "random",
  #   summaryFunction = twoClassSummary,
  #   classProbs = TRUE)
  # 
  # 
  # # preProcOptions=list(thresh=0.99))
  # cat(sprintf('\n\nBeginning to train the model...\n\n'))
  # 
  # model <- train(x = ann_inputs_center_scale,
  #                    y = output,
  #                    method = 'mxnetAdam',
  #                    preProc = NULL, #c("center","scale"), #already normalized
  #                    trControl = ctrl.mxnet,
  #                    tuneGrid = mxnetAdam.params,
  #                    num.round = 1000,
  #                    maximize = FALSE
  #                    )
  # 
  # print(model)
  # model$finalModel
  # #graph.viz(model$symbol)
  # 
  # #png(filename = "hyp_grid_v2.png")
  # plot(model) #only use with hyperparameter grid ON
  # #dev.off()
  # 
  # ann_name <- paste0("ann_v",iteration,".ann")
  # save(model, file=ann_name)
  # 
  # stopCluster(workers)
  # 
  # #predict outputs given a model and a dataset
  # #predictions <- predict(model, ann_inputs)

  


  
  ### Predictions using any model ###
    
  #predict outputs given a model and a dataset
  predictions <- as.vector(predict(model, ann_inputs_center_scale, ctx = NULL,
                                   array.batch.size = batch_size,
                                   array.layout = "rowmajor",
                                   allow.extra.params = FALSE))
  
  #predictions <- round(predictions, digits = 2)
  

  #### Graph for training cases ####

  #training cases
  prediction_training <- data.frame(simulation = output,
                                    prediction = round(predictions, digits = 2))
  
  obs <- prediction_training$prediction
  sim <- prediction_training$simulation
  #norm="sd" or "maxmin"
  stats.train <- as.data.frame(t(gof(sim, obs, na.rm=TRUE, norm="maxmin", s=c(1,1,1), lQ.thr=0.7, hQ.thr=0.2, digits=3)))
  print("Training cases - ANN stats")
  print(stats.train)
  cat("\n")
  
  #plot ANN accuracy (seen cases)
  training_cases <- ggplot(prediction_training, aes(x=prediction_training$simulation, y=prediction_training$prediction)) +
    geom_point(size=graph$dot_size, col='blue', alpha=graph$alpha) +
    labs(title = paste0(nrow(prediction_training)," casos de treinamento - Iteração ",l)) +
    xlab("Carga térmica de refrigeração simulada (kWh/m²)") +
    ylab("Carga térmica de refrigeração predita (kWh/m²)") +
    geom_abline(col='black') + xlim(axis_min,axis_max) + ylim(axis_min,axis_max) +
    annotate("text", hjust=0, x = axis_min, y = axis_max, label="R2 =") +
    annotate("text", hjust=0, x = (axis_min + 0.08*axis_max), y=axis_max, label=stats.train$R2) +
    annotate("text", hjust=0, x = axis_min, y = 0.95*axis_max, label="RMSE =") +
    annotate("text", hjust=0, x = (axis_min + 0.15*axis_max), y = 0.95*axis_max, label=stats.train$RMSE) +
    annotate("text", hjust=0, x = axis_min, y = 0.90*axis_max, label="NRMSE(%) =") +
    annotate("text", hjust=0, x = (axis_min + 0.24*axis_max), y = 0.90*axis_max, label=stats.train$NRMSE)
  
  print(training_cases)
  
  ggsave(filename = paste0("training_",iteration,".png"),
         plot = last_plot(),
         scale = 1,
         width = 5,
         height = 5,
         units = "in")
  
  dev.off()
  
  
  #### Graph for evaluation cases #### 

  
  #predict outputs given a model and a dataset
  unseen_predictions <- as.vector(predict(model, unseen_ann_inputs_center_scale, ctx = NULL,
                                          array.batch.size = batch_size,
                                          array.layout = "rowmajor",
                                          allow.extra.params = FALSE))
  
  #validation cases
  prediction_validation <- data.frame(simulation = unseen_output,
                                prediction = round(unseen_predictions, digits = 2))
  
  obs <- prediction_validation$prediction
  sim <- prediction_validation$simulation
  #norm="sd" or "maxmin"
  stats.validation <- as.data.frame(t(gof(sim, obs, na.rm=TRUE, norm="maxmin", s=c(1,1,1), lQ.thr=0.7, hQ.thr=0.2, digits=3)))
  print("Validation cases - ANN stats")
  print(stats.validation)
  cat("\n")
  
  #plot ANN accuracy (evaluation cases)
  validation_cases <- ggplot(prediction_validation, aes(x=prediction_validation$simulation, y=prediction_validation$prediction)) +
    geom_point(size=graph$dot_size, col='red', alpha=graph$alpha) +
    labs(title = paste0("Avaliação - N casos: ",nrow(prediction_validation)," - Iteracao: ",l)) +
    xlab("Carga térmica de refrigeração simulada (kWh/m²)") +
    ylab("Carga térmica de refrigeração predita (kWh/m²)") +
    geom_abline(col='black') + xlim(axis_min,axis_max) + ylim(axis_min,axis_max) +
    annotate("text", hjust=0, x = axis_min, y = axis_max, label="R2 =") +
    annotate("text", hjust=0, x = (axis_min + 0.08*axis_max), y=axis_max, label=stats.validation$R2) +
    annotate("text", hjust=0, x = axis_min, y = 0.95*axis_max, label="RMSE =") +
    annotate("text", hjust=0, x = (axis_min + 0.15*axis_max), y = 0.95*axis_max, label=stats.validation$RMSE) +
    annotate("text", hjust=0, x = axis_min, y = 0.90*axis_max, label="NRMSE(%) =") +
    annotate("text", hjust=0, x = (axis_min + 0.24*axis_max), y = 0.90*axis_max, label=stats.validation$NRMSE)
  
  print(validation_cases)
  
  ggsave(filename = paste0("validation_",iteration,".png"),
         plot = last_plot(),
         scale = 1,
         width = 5,
         height = 5,
         units = "in")
  
  dev.off()
  
  
  
  #### "So far" cases graph #### 
  
  # prediction_so_far <- data.frame(simulation = filtered_df$cooling_loads,
  #                                 prediction = predict(nnetFit, filtered_df))
  # 
  # obs <- prediction_so_far$prediction
  # sim <- prediction_so_far$simulation
  # #norm="sd" or "maxmin"
  # stats.incre <- as.data.frame(t(gof(sim, obs, na.rm=TRUE, norm="maxmin", s=c(1,1,1), lQ.thr=0.7, hQ.thr=0.2, digits=3)))
  # print("Incremental cases - ANN stats")
  # print(stats.incre)
  # cat("\n")
  # 
  # #plot ANN accuracy (so far cases)
  # cases_so_far <- ggplot(prediction_so_far, aes(x=prediction_so_far$simulation, y=prediction_so_far$prediction)) +
  #   geom_point(size=graph$dot_size, col='darkblue', alpha=graph$alpha) +
  #   labs(title = paste0("Total - N? casos: ",nrow(prediction_so_far)," - Itera??o: ",l)) +
  #   xlab("Carga t?rmica de resfriamento simulada (kWh/m2.m?s)") +
  #   ylab("Carga t?rmica de resfriamento predita (kWh/m2.m?s)") +
  #   geom_abline(col='black') + xlim(axis_min,axis_max) + ylim(axis_min,axis_max) +
  #   annotate("text", hjust=0, x = axis_min, y = axis_max, label="R? =") +
  #   annotate("text", hjust=0, x = (axis_min + 0.08*axis_max), y=axis_max, label=stats.incre$R2) +
  #   annotate("text", hjust=0, x = axis_min, y = 0.95*axis_max, label="RMSE =") +
  #   annotate("text", hjust=0, x = (axis_min + 0.15*axis_max), y = 0.95*axis_max, label=stats.incre$RMSE) +
  #   annotate("text", hjust=0, x = axis_min, y = 0.90*axis_max, label="NRMSE(%) =") +
  #   annotate("text", hjust=0, x = (axis_min + 0.24*axis_max), y = 0.90*axis_max, label=stats.incre$NRMSE)
  # 
  # print(cases_so_far)
  # 
  # ggsave(filename = paste0("so_far_",iteration,".png"),
  #        plot = last_plot(),
  #        scale = 1,
  #        width = 5,
  #        height = 5,
  #        units = "in")
  # dev.off()
  # 
  # #plot training, validation and "so far" cases in the same graph
  # training_and_validation_cases <- grid.arrange(training_cases, validation_cases, cases_so_far, nrow = 1)
  # ggsave(filename = paste0("training_validation_sofar_",iteration,".png"),
  #        plot = training_and_validation_cases,
  #        scale = 1,
  #        width = 15,
  #        height = 5,
  #        units = "in")
  # dev.off()
  
  #rm(model) #to avoid overwriting files
  
  
  #### Iteration count ####
  iteration <- iteration + 1
  
  
}


#### Other plots ####

#variable importance and hyperparameters grid plots are still missing!!!!!!!!!!!!!!!!!!!!!!


#prediction as final performance
ann_list <- list.files(path = path_incremental, pattern = ".params$", full.names = T, recursive = T) #works for MXNET models
#ann_list <- list.files(path = path_incremental, pattern = ".ann$", full.names = T, recursive = T) #works for caret models

ann_list

#read test dataset
test_dataset <- read.csv(file = "C:\\Users\\LabEEE_1-3\\Desktop\\test_dataset\\iter_000004\\filtered_df.csv")


#create two empty lists to store stats.final and stats.test of each iteration
stats_final_list <- list()
stats_test_list <- list()

#set path
setwd(path_incremental)


for (ann in 1:length(ann_list)) {
  
  ann_iter = ann - 1
  
  #Load model from file
  
  #caret files
  #load(ann_list[ann])
  
  #mxnet files
  #fancy way
  model <- mx.model.load(prefix = paste0(sub('-([^-]*)$', '',ann_list[ann])), iteration = epochs)
  # #old way
  # model <- mx.model.load(prefix = paste0(path_incremental,"/",
  #                                        iter_folder_names[ann],
  #                                        "/ann_iter_",ann_iter), iteration = epochs)
  
  
  all_inputs <- data.matrix(subset(filtered_df, select= -c(cooling_loads)))
  all_inputs_center_scale <- scale(all_inputs, center = T, scale = T)
  all_output <- filtered_df$cooling_loads
  
  test_inputs <- data.matrix(subset(test_dataset, select= -c(cooling_loads)))
  test_inputs_center_scale <- scale(test_inputs, center = T, scale = T)
  test_output <- test_dataset$cooling_loads
  

  ##only perimetral zones results (inputs and outputs to drible center and scale processes) 06/02/2020 TESTS
  ##inputs
  #test_inputs_center_scale <- test_inputs_center_scale[test_inputs_center_scale[ ,"zone_facade_length"] > 0, ]
  ##outputs
  #test_dataset <- subset(test_dataset, test_dataset$zone_facade_length != 0)
  #test_output <- test_dataset$cooling_loads
  
  
  #predictions
  all_predictions <- as.vector(predict(model, all_inputs_center_scale, ctx = NULL,
                                       array.batch.size = batch_size,
                                       array.layout = "rowmajor",
                                       allow.extra.params = FALSE))
  
  test_predictions <- as.vector(predict(model, test_inputs_center_scale, ctx = NULL,
                                       array.batch.size = batch_size,
                                       array.layout = "rowmajor",
                                       allow.extra.params = FALSE))
  

  #all cases
  prediction_final <- data.frame(simulation = all_output, 
                                 prediction = round(all_predictions, digits = 2))
  
  obs_final <- prediction_final$prediction
  sim_final <- prediction_final$simulation
  
  #test cases
  prediction_test <- data.frame(simulation = test_output, 
                                 prediction = round(test_predictions, digits = 2))
  
  obs_test <- prediction_test$prediction
  sim_test <- prediction_test$simulation
  
  #norm="sd" or "maxmin"
  stats.final <- as.data.frame(t(gof(sim_final, obs_final, na.rm=TRUE, norm="maxmin", 
                                     s=c(1,1,1), lQ.thr=0.7, hQ.thr=0.2, digits=3)))
  
  print("Final cases - ANN stats")
  print(stats.final)
  cat("\n")
  
  
  stats.test <- as.data.frame(t(gof(sim_test, obs_test, na.rm=TRUE, norm="maxmin", 
                                    s=c(1,1,1), lQ.thr=0.7, hQ.thr=0.2, digits=3)))
  
  print("Test cases - ANN stats")
  print(stats.test)
  cat("\n")
  
  
  #store stats.final data in the previously created list
  stats_final_list[[ann]] <- stats.final
  
  stats_test_list[[ann]] <- stats.test
  
  
  #plot ANN accuracy (seen cases)
  final_cases <- ggplot(prediction_final, aes(x=prediction_final$simulation, y=prediction_final$prediction)) +
    geom_point(size=graph$dot_size, col='darkblue', alpha=graph$alpha) +
    labs(title = paste0(nrow(filtered_df)," casos vistos - ANN ", ann_iter)) +
    xlab("Carga térmica de refrigeração simulada (kWh/m²)") +
    ylab("Carga térmica de refrigeração predita (kWh/m²)") +
    geom_abline(col='black') + xlim(axis_min,axis_max) + ylim(axis_min,axis_max) +
    annotate("text", hjust=0, x = axis_min, y = axis_max, label="R2 =") +
    annotate("text", hjust=0, x = (axis_min + 0.08*axis_max), y=axis_max, label=stats.final$R2) +
    annotate("text", hjust=0, x = axis_min, y = 0.95*axis_max, label="RMSE =") +
    annotate("text", hjust=0, x = (axis_min + 0.15*axis_max), y = 0.95*axis_max, label=stats.final$RMSE) +
    annotate("text", hjust=0, x = axis_min, y = 0.90*axis_max, label="NRMSE(%) =") +
    annotate("text", hjust=0, x = (axis_min + 0.24*axis_max), y = 0.90*axis_max, label=stats.final$NRMSE)
  
  print(final_cases)
  
  ggsave(filename = paste0("performance_train_validation_",sprintf("%04d", ann_iter),".png"),
         plot = last_plot(),
         scale = 1,
         width = 5,
         height = 5,
         units = "in")
  dev.off()
  
  #plot ANN accuracy (unseen cases)
  test_cases <- ggplot(prediction_test, aes(x=prediction_test$simulation, y=prediction_test$prediction)) +
    geom_point(size=graph$dot_size, col='darkgreen', alpha=graph$alpha) +
    labs(title = paste0(nrow(test_dataset)," casos não vistos - ANN ", ann_iter)) +
    xlab("Carga térmica de refrigeração simulada (kWh/m²)") +
    ylab("Carga térmica de refrigeração predita (kWh/m²)") +
    geom_abline(col='black') + xlim(axis_min,axis_max) + ylim(axis_min,axis_max) +
    annotate("text", hjust=0, x = axis_min, y = axis_max, label="R2 =") +
    annotate("text", hjust=0, x = (axis_min + 0.08*axis_max), y=axis_max, label=stats.test$R2) +
    annotate("text", hjust=0, x = axis_min, y = 0.95*axis_max, label="RMSE =") +
    annotate("text", hjust=0, x = (axis_min + 0.15*axis_max), y = 0.95*axis_max, label=stats.test$RMSE) +
    annotate("text", hjust=0, x = axis_min, y = 0.90*axis_max, label="NRMSE(%) =") +
    annotate("text", hjust=0, x = (axis_min + 0.24*axis_max), y = 0.90*axis_max, label=stats.test$NRMSE)
  
  print(test_cases)
  
  ggsave(filename = paste0("performance_test_",sprintf("%04d", ann_iter),".png"),
         plot = last_plot(),
         scale = 1,
         width = 5,
         height = 5,
         units = "in")
  dev.off()
  
  #png(filename = paste0("var_imp_",ann_iter,".png"))
  #ggplot(varImp(model)) #ggplot plots of objects with class "varImp.train"
  #print(varImp(model))
  #dev.off()
  
}

gc()




#plot graphs with stats indicators evolution for seen (training + validation) and unseen (test) cases
#seen
stats_df_final <- bind_rows(stats_final_list, .id="iter")
stats_df_final$iter <- as.numeric(stats_df_final$iter)
stats_df_final$iter <- stats_df_final$iter-1

stats_final_plot <- ggplot(stats_df_final, aes(iter,`NRMSE %`)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = pretty_breaks()) + ylim(0,20)

print(stats_final_plot)

ggsave(filename = paste0("stats_final_plot.png"),
       plot = last_plot(),
       scale = 1,
       width = 5,
       height = 5,
       units = "in")

dev.off()

#unseen
stats_df_test <- bind_rows(stats_test_list, .id="iter")
stats_df_test$iter <- as.numeric(stats_df_test$iter)
stats_df_test$iter <- stats_df_test$iter-1

stats_test_plot <- ggplot(stats_df_test, aes(iter,`NRMSE %`)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = pretty_breaks()) + ylim(0,20)

print(stats_test_plot)

ggsave(filename = paste0("stats_test_plot.png"),
       plot = last_plot(),
       scale = 1,
       width = 5,
       height = 5,
       units = "in")

dev.off()



#list ANN graphs and make animation
ann_graph <- c("performance_train_validation_","performance_test_")
#ann_graph <- c("training_", "test_", "so_far_", "training_test_sofar_")
setwd(path_incremental)

for (g in 1:length(ann_graph)) {
  
  list.files(path = path_incremental,
             pattern = paste0(ann_graph[g],".*\\.png$"),
             full.names = T,
             recursive = T) %>%
    
    map(image_read) %>% # reads each path file
    image_join() %>% # joins images
    image_animate(fps=1) %>%
    image_write(paste0("ann_evolution_",ann_graph[g],".gif"))
}


#list correlation graphs and make animation
ann_graph <- c("corr_plot_matrix")
#ann_graph <- c("training_", "test_", "so_far_", "training_test_sofar_")
setwd(path_incremental)

for (g in 1:length(ann_graph)) {
  
  list.files(path = path_incremental,
             pattern = paste0(ann_graph[g],".*\\.png$"),
             full.names = T,
             recursive = T) %>%
    
    map(image_read) %>% # reads each path file
    image_join() %>% # joins images
    image_animate(fps=1) %>%
    image_write(paste0("ann_evolution_",ann_graph,".gif"))
}




#### FULL BUILDING PREDICTION ####

ann_list <- list.files(path = path_incremental, pattern = ".params$", full.names = T, recursive = T) #works for MXNET models
#ann_list <- list.files(path = path_incremental, pattern = ".ann$", full.names = T, recursive = T) #works for caret models

ann_list

#read a big dataset to avoid problems with center/scale
fullb_dataset <- read.csv(file = "C:\\Users\\LabEEE_1-3\\Desktop\\incremental\\iter_000019\\filtered_df.csv")
fullb_dataset_v2 <- read.csv(file = "C:\\Users\\LabEEE_1-3\\Desktop\\filtered_df_v2.csv")
#fullb_dataset_v2 <- fullb_dataset_v2[ ,3:29]
#fullb_dataset_v2[] <- lapply(fullb_dataset_v2, function(x) as.numeric(x))
fullb_dataset <- rbind(fullb_dataset, fullb_dataset_v2)


#create an empty list to store fullb.stats of each iteration
stats_fullb_list <- list()

#must be equal to development values
epochs <- 1000
batch_size <- 32

#set path
setwd(path_incremental)


for (ann in 1:length(ann_list)) {
  
  ann_iter = ann - 1
  
  #Load model from file
  
  #caret files
  #load(ann_list[ann])
  
  #mxnet files
  #fancy way
  model <- mx.model.load(prefix = paste0(sub('-([^-]*)$', '',ann_list[ann])), iteration = epochs)
  # #old way
  # model <- mx.model.load(prefix = paste0(path_incremental,"/",
  #                                        iter_folder_names[ann],
  #                                        "/ann_iter_",ann_iter), iteration = epochs)
  
  
  fullb_inputs <- data.matrix(subset(fullb_dataset, select= -c(cooling_loads)))
  fullb_inputs_center_scale <- scale(fullb_inputs, center = T, scale = T)
  fullb_output <- fullb_dataset$cooling_loads
  
  #adjust to few results
  fullb_inputs_center_scale <- tail(fullb_inputs_center_scale, length(fullb_dataset_v2[, 1]))
  fullb_output <- tail(fullb_output, length(fullb_dataset_v2[, 1]))
  

  
  
  #predictions
  fullb_predictions <- as.vector(predict(model, fullb_inputs_center_scale, ctx = NULL,
                                        array.batch.size = batch_size,
                                        array.layout = "rowmajor",
                                        allow.extra.params = FALSE))
  
  

  #full building cases
  prediction_fullb <- data.frame(simulation = fullb_output, 
                                prediction = round(fullb_predictions, digits = 2))
  
  obs_fullb <- prediction_fullb$prediction
  sim_fullb <- prediction_fullb$simulation
  

  stats.fullb <- as.data.frame(t(gof(sim_fullb, obs_fullb, na.rm=TRUE, norm="maxmin", 
                                    s=c(1,1,1), lQ.thr=0.7, hQ.thr=0.2, digits=3)))
  
  print("Full building cases - ANN stats")
  print(stats.fullb)
  cat("\n")
  
  
  #store stats.fullb data in the previously created list

  stats_fullb_list[[ann]] <- stats.fullb
  
  
  #graphic parameters
  graph$alpha <- 0.2
  graph$dot_size <- 1
  
  axis_min <- -5
  axis_max <- 50  

  #plot ANN accuracy (full building cases)
  fullb_cases <- ggplot(prediction_fullb, aes(x=prediction_fullb$simulation, y=prediction_fullb$prediction)) +
    geom_point(size=graph$dot_size, col='darkgreen', alpha=graph$alpha) +
    labs(title = paste0(nrow(fullb_inputs_center_scale)," casos edificação inteira - Rio de Janeiro - ANN ", ann_iter)) +
    xlab("Carga térmica de refrigeração simulada (kWh/m²)") +
    ylab("Carga térmica de refrigeração predita (kWh/m²)") +
    geom_abline(col='black') + xlim(axis_min,axis_max) + ylim(axis_min,axis_max) +
    annotate("text", hjust=0, x = axis_min, y = axis_max, label="R2 =") +
    annotate("text", hjust=0, x = (axis_min + 0.08*axis_max), y=axis_max, label=stats.fullb$R2) +
    annotate("text", hjust=0, x = axis_min, y = 0.95*axis_max, label="RMSE =") +
    annotate("text", hjust=0, x = (axis_min + 0.15*axis_max), y = 0.95*axis_max, label=stats.fullb$RMSE) +
    annotate("text", hjust=0, x = axis_min, y = 0.90*axis_max, label="NRMSE(%) =") +
    annotate("text", hjust=0, x = (axis_min + 0.24*axis_max), y = 0.90*axis_max, label=stats.fullb$NRMSE)
  
  print(fullb_cases)
  
  ggsave(filename = paste0("performance_fullb_",sprintf("%04d", ann_iter),".png"),
         plot = last_plot(),
         scale = 1,
         width = 6,
         height = 6,
         units = "in")
  dev.off()
  

}

gc()






# #secret moth EDA
# moth_df <- cbind(test_dataset,prediction_test)
# 
# inc.plot <- ggpairs(data = moth_df,
#              title = "EDA",
#              #upper = list(continuous = "cor", combo = "box_no_facet"),
#              diag=list(continuous = "densityDiag"),
#              #lower = list(continuous = wrap(lower_function)),
#              axisLabels = "none")
#
#print(inc.plot)


### TO-DO LIST
#sampling methods may be less accurate as long as dimensions grow in df!!!! (visual theory)
#repeat until the learning curve (training and test) stabilizes 
#variable importance and hyperparameters grid plots
