#### Incremental sampling code - Leonardo Mazzaferro ####

####LIBRARIES####
library(caret)
library(corrplot)
library(data.table)
library(deldir) #voronoi diagrams
library(DiceDesign) #discrepancy
library(doParallel)
library(dplyr) #use for fixing up data
library(FCNN4R) #fast compressed neural networks
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
library(magick) #animate/read pngs
library(MASS)
library(NeuralNetTools)
library(plotly)
library(plyr)
library(pse)
library(purrr) #mapping over a function
library(randtoolbox)
library(readr) #reading in data/csv
library(RColorBrewer) #color palettes
library(Rmisc)
library(rlist)
library(sampling)
library(snow)
library(viridis) #even more color palettes

#### DEFINE AND SET PATHS ####
path_discrepancy <- "C:/Users/LabEEE_1-3/Dropbox/LEO/2020/discrepancia"
#path_discrepancy <- "C:/Users/LabEEE_1-3/Desktop/discrepancy"
#path_sample <- "C:/Users/LabEEE_1-3/Desktop/incremental/sample_creation"

setwd(path_discrepancy)

#### INCREMENTAL SAMPLING HYPER-PARAMETERS ####
set.seed(7) #reproducible results

hyp <- list()
hyp$starter_points <- 100 #minimum required: 2 points (for sampling) or the number of dimensions (for discrepancy)
hyp$incremental_points <- 200
hyp$final_points <- 500

hyp$n_iter <- (hyp$final_points - hyp$starter_points)/hyp$incremental_points 
#
hyp$n_climates_per_iter <- 1
hyp$fps <- 1

graph <- list()
graph$alpha <- 0.2
graph$dot_size <- 0.5

min_number_of_dimensions <- 20
max_number_of_dimensions <- 20
#dimension_increment <- 5 # NEW WAY... precisa de ajustes no FOR para funcionar bem

### SCATTERPLOT MATRIX FUNCTIONS ###
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


#### SAMPLING METHODS ####
methods_vector <- c("Random LHS"
                    , "Improved LHS"
                    , "Optimum LHS"
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

#dataframe with discrepancy evolution ... CAUTION: number of points cannot be lower than number of dimensions
df.discr <- data.frame(matrix(0, ncol = number_of_methods,
                              nrow = (hyp$n_iter + 1)*(max_number_of_dimensions-min_number_of_dimensions + 1))) #OLD WAY

# #dataframe with discrepancy evolution ... CAUTION: number of points cannot be lower than number of dimensions
# df.discr <- data.frame(matrix(0, ncol = number_of_methods, 
#                               nrow = (hyp$n_iter + 1)*(1 + (max_number_of_dimensions-min_number_of_dimensions)/dimension_increment))) # NEW WAY... precisa de ajustes no FOR para funcionar bem


colnames(df.discr) <- methods_vector
df.discr$Iteration <- 0:hyp$n_iter



df.discr$SampleSize <- rep(seq(from = hyp$starter_points,
                               to = hyp$final_points,
                               by = hyp$incremental_points),
                           times = max_number_of_dimensions - min_number_of_dimensions + 1) #OLD WAY


# df.discr$SampleSize <- rep(seq(from = hyp$starter_points,
#                                to = hyp$final_points,
#                                by = hyp$incremental_points),
#                            times = 1 + (max_number_of_dimensions - min_number_of_dimensions)/dimension_increment) # NEW WAY... precisa de ajustes no FOR para funcionar bem


df.discr$Dimensions <- rep(min_number_of_dimensions:max_number_of_dimensions, each = hyp$n_iter + 1) #times > each #OLD WAY

# df.discr$Dimensions <- rep(seq(from = min_number_of_dimensions,
#                                to = max_number_of_dimensions,
#                                by = dimension_increment), each = hyp$n_iter + 1) # NEW WAY... precisa de ajustes no FOR para funcionar bem


#set discrepancy type
discrepancy_type <- "C2"
discrepancy_type_name <- " "
discrepancy_type_name <- ifelse(discrepancy_type == "L2", discrepancy_type_name <- "L2", discrepancy_type_name)
discrepancy_type_name <- ifelse(discrepancy_type == "C2", discrepancy_type_name <- "Centered L2", discrepancy_type_name)

#make cluster
#clusterApply(cl = NULL, x, fun, ...)

#reproducible results
set.seed(777)

for (dimensions_number in min_number_of_dimensions:max_number_of_dimensions) {
  
  #first iteration
  for (h in 1:number_of_methods) {
    
    #set sampling method
    sampling_method <- methods_vector[[h]]
    #print sampling method
    print(sampling_method)
    #define row to add results in dataframe
    df_row <- 1 + (hyp$n_iter + 1)*(dimensions_number - min_number_of_dimensions)
    
    if (sampling_method == "Random LHS") {
      data <- randomLHS(n=hyp$starter_points,
                        k=dimensions_number,
                        preserveDraw=FALSE)
      
      df.discr$`Random LHS`[df_row] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
    }
    
    
    if (sampling_method == "Improved LHS") {
      data <- improvedLHS(n=hyp$starter_points,
                          k=dimensions_number,
                          dup=1)
      
      df.discr$`Improved LHS`[df_row] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
    }
    
    
    if (sampling_method == "Optimum LHS") {
      data <- improvedLHS(n=hyp$starter_points,
                          k=dimensions_number)
      
      df.discr$`Optimum LHS`[df_row] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
    }
    
    
    if (sampling_method == "Sobol") {
      data <- sobol(n=hyp$starter_points,
                    dim=dimensions_number,
                    init=TRUE,
                    scrambling=0,
                    seed=4711,
                    normal=FALSE)
      
      df.discr$`Sobol`[df_row] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
    }
    
    
    if (sampling_method == "Halton") {
      data <- halton(n=hyp$starter_points,
                     dim=dimensions_number,
                     init=TRUE,
                     normal=FALSE,
                     usetime=FALSE)
      
      df.discr$`Halton`[df_row] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
    }
    
    
    if (sampling_method == "Torus") {
      data <- torus(n=hyp$starter_points,
                    dim=dimensions_number,
                    init=TRUE,
                    mixed=FALSE,
                    normal=FALSE,
                    usetime=FALSE)
      
      df.discr$`Torus`[df_row] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
    }
    
    
    
    #for each iteration afterwards (starter + n_iter ... until the final points are reached)
    for (i in 0:hyp$n_iter) {
      
      #calculate number of points
      current_points <- hyp$starter_points + i*hyp$incremental_points
      #print points
      print(current_points)
      
      if (i != 0 & sampling_method == "Random LHS") {
        data <- augmentLHS(data,
                           m=hyp$incremental_points)
        
        df.discr$`Random LHS`[df_row + i] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
        ls_sample_mat[[h]] <- data
        names(ls_sample_mat)[[h]] <- methods_vector[h]
      }
      
      
      if (i != 0 & sampling_method == "Improved LHS") { #conferir este método... pois apresenta results uma ordem pior!!)
        # data <- optAugmentLHS(data,
        #                       m=hyp$incremental_points,
        #                       mult=hyp$dimensions_number)
        data <- augmentLHS(data,
                           m=hyp$incremental_points) #testing
        
        df.discr$`Improved LHS`[df_row + i] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
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
        
        df.discr$`Optimum LHS`[df_row + i] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
        ls_sample_mat[[h]] <- data
        names(ls_sample_mat)[[h]] <- methods_vector[h]
      }
      
      
      if (i != 0 & sampling_method == "Sobol") {
        new_data <- sobol(n=hyp$incremental_points,
                          dim=dimensions_number,
                          init=FALSE,
                          scrambling=0,
                          seed=4711,
                          normal=FALSE)
        
        data <- rbind(data, new_data)
        
        df.discr$`Sobol`[df_row + i] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
        ls_sample_mat[[h]] <- data
        names(ls_sample_mat)[[h]] <- methods_vector[h]
      }
      
      
      if (i != 0 & sampling_method == "Halton") {
        new_data <- halton(n=hyp$incremental_points,
                           dim=dimensions_number,
                           init=FALSE,
                           normal=FALSE,
                           usetime=FALSE)
        data <- rbind(data, new_data)
        
        df.discr$`Halton`[df_row + i] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
        ls_sample_mat[[h]] <- data
        names(ls_sample_mat)[[h]] <- methods_vector[h]
      }
      
      
      if (i != 0 & sampling_method == "Torus") {
        new_data <- torus(n=hyp$incremental_points,
                          dim=dimensions_number,
                          init=FALSE,
                          mixed=FALSE,
                          normal=FALSE,
                          usetime=FALSE)
        
        data <- rbind(data, new_data)
        
        df.discr$`Torus`[df_row + i] <- as.numeric(discrepancyCriteria(data, type = discrepancy_type))
        ls_sample_mat[[h]] <- data
        names(ls_sample_mat)[[h]] <- methods_vector[h]
      }
      
      
      n.points <- sprintf("%05d", current_points) #add zeros before number
      image.name <- paste0(dimensions_number," dimensions ",sampling_method," ",n.points," points",".png")
      
      df.data <- as.data.frame(data)
      colnames(df.data) <- c(paste0("var",1:dimensions_number))
      
      #GGPLOT plots with distributions
      # inc.plot <- ggpairs(data = df.data,
      #                     title = paste(sampling_method,"/ iteration",
      #                                   i,"/ number of points:",n.points),
      #                     upper = list(continuous = "cor", combo = "box_no_facet"),
      #                     diag=list(continuous = "densityDiag"),
      #                     lower = list(continuous = wrap(lower_function)),
      #                     axisLabels = "none")
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
    
    
  }
  
  
  
}

#write csv
write.csv(df.discr, 
          file = paste0("discrepancy_values_",
                                  min_number_of_dimensions,
                                  "d-",
                                  max_number_of_dimensions,
                                  "d_",
                                  hyp$starter_points,
                                  "p-",
                                  hyp$final_points,
                                  "p",
                                  ".csv"),
          row.names = FALSE)



#load csv
df.discr <- read.csv(file = paste0("discrepancy_values_",
                                   min_number_of_dimensions,
                                   "d-",
                                   max_number_of_dimensions,
                                   "d_",
                                   hyp$starter_points,
                                   "p-",
                                   hyp$final_points,
                                   "p",
                                   ".csv")
                     )


## MODULE 4: Calculate discrepancy and plot the optimal methods for each sample size
# #voronoi tesselation diagrams
# x <- cbind(df.discr$SampleSize, 
#            df.discr$SampleSize, 
#            #df.discr$SampleSize, #removed... it was too slow!!!
#            df.discr$SampleSize, 
#            df.discr$SampleSize, 
#            df.discr$SampleSize)
# 
# y <- cbind(df.discr$`Random LHS`, 
#            df.discr$`Improved LHS`, 
#            #df.discr$`Optimum LHS`, #removed... it was too slow!!!
#            df.discr$Sobol,
#            df.discr$Halton,
#            df.discr$Torus)
# 
# matplot(x,y,
#         type="p",
#         xlab = "Sample Size", ylab = paste0("Discrepancy for ", dimensions_number, " dimensions"))

#find tesselation coding file (I wrote one hehe) and try to implement it here!


#2d plot
library(reshape2)


for (dim in min_number_of_dimensions:max_number_of_dimensions) {
  
  print(dim)
  df.dim <- subset(df.discr, Dimensions == dim)
  df.dim$Dimensions <- NULL
  df.dim$Iteration <- NULL
  
  df <- melt(df.dim, id.vars = "SampleSize")
  
  colnames(df) <- c("Tamanho", "Amostragem", "Discrepância")
  
  g <- ggplot(df, aes(Tamanho, Discrepância, col=Amostragem)) + 
    geom_point(size = 2, aes(shape=Amostragem)) +
    ylim(0.1, 1) +
    labs(subtitle=paste0("Resultado para ", dim, " dimensões"), 
         #title="Sample size vs Discrepancy",
         #caption = "Source: o autor",
         x = "Tamanho da amostra",
         #y = discrepancy_type_name, 
         y = "Discrepância")

  print(g)
  
  ggsave(g, file=paste0("discrepancy_plot_", dim, "_dimensions_", hyp$final_points,"_points",".png"), 
         width = 16, height = 8, units = "cm")
  
}



#3d interactive plot
#py <- plotly()
#set_credentials_file(username = 'your_username', key = 'your_key')

#p <- plot_ly(z = as.matrix(df.discr[ ,c(1,7,8)]), type = "surface")


# #first try (dotted 3d graph) - quite good actually... i can add other sampling methods by changing dot colour
# df.discr$Iteration <- NULL
# 
# p <- plot_ly(data = df.discr,
#              x = ~SampleSize,
#              y = ~Dimensions,
#              z = ~Sobol) #, type = "surface"
# 
# p


#second try
# p <- plot_ly() %>%
#   add_trace(data = df.discr,  x=df.discr$SampleSize, y=df.discr$Dimensions, z=df.discr$Sobol, type="mesh3d" )
# 
# p

#Error: Trace type must be one of the following:
#'scatter', 'bar', 'box', 'heatmap', 'histogram', 'histogram2d', 'histogram2dcontour',
#'pie', 'contour', 'scatterternary', 'violin', 'scatter3d', 'surface', 'mesh3d', 'cone',
#''streamtube', 'scattergeo', 'choropleth', 'scattergl', 'splom', 'pointcloud', 'heatmapgl',
#''parcoords', 'scattermapbox', 'sankey', 'table', 'carpet', 'scattercarpet', 'contourcarpet',
#''ohlc', 'candlestick', 'scatterpolar', 'scatterpolargl', 'area'



#third try (

# x_vec <- unique(df.discr$SampleSize)
# x_vec
# 
# y_vec <- unique(df.discr$Dimensions)
# y_vec
# 
# z_vec <- df.discr$Sobol
# 
# z_mat <- matrix(z_vec,
#                 nrow = length(x_vec),
#                 ncol = length(y_vec))
# z_mat
# 
# 
# axx <- list(
#   title = "Number of dimensions",
#   nticks = length(x_vec)
# )
# 
# axy <- list(
#   title = "Number of samples",
#   nticks = length(x_vec)
# )
# 
# axz <- list(
#   title = "L2 Discrepancy",
#   nticks = 4
# )
# 
# p <- plot_ly() %>%
#   add_surface(z = z_mat)%>%
#   layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
# 
# p






# ## TEST BELOW
# 
# #simulating random data
# x1 <- rnorm(n = 100, mean = 5, sd = 1)
# x2 <- rnorm(n = 100, mean = 10, sd = 2.5)
# 
# y1 <- rnorm(n = 1000, mean = 4, sd = 3)
# y2 <- rnorm(n = 1000, mean = 100, sd = 2.5)
# 
# #creating two-dimensional kernal density estimation 
# data1 <- kde2d(x = x1, y = x2, n = 25) # n = number of grids)
# data2 <- kde2d(x = y1, y = y2, n = 25) # n = number of grids)
# 
# 
# p <- plot_ly() %>%
#   add_surface(z = data1$z) %>%
#   add_surface(z = data2$z)
# 
# p <- p %>% layout(
#   title = "Button Restyle",
#   updatemenus = list(
#     list(
#       type = "buttons",
#       y = 0.8,
#       buttons = list(
#         
#         list(method = "restyle",
#              args = list("visible", c(F,T)),
#              label = "group1"),
#         
#         
#         list(method = "restyle",
#              args = list("visible", c(T,F)),
#              label = "group2")))
#   ))
# 
# p
# 
# 
# #second example
# 
# #Create Plotly object
# plot_ly(showscale = FALSE) %>%
#   
#   #First rectangle
#   add_surface(x = c(10, 60),
#               y = c(10, 50),
#               z = matrix(160, nrow = 2, ncol = 2)) %>%
#   
#   #Second rectangle
#   add_surface(x = c(10, 60),
#               y = c(10, 50),
#               z = matrix(180, nrow = 2, ncol = 2))
# 

