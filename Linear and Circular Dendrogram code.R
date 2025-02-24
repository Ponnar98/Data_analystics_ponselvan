library(readxl)
library(dplyr) # it has '>%>' pipe function
library(dendextend) # it has set() function

sdata <- read_excel("P:/Sheela mam/Cluster analysis/Data analysis.xls", 
                    sheet = "Mean data")

sdata <- sdata %>% 
          as.data.frame() %>% 
          `rownames<-`(sdata$Clone) %>%
          select(,c(-1,-2))

  ###############################################################
  # Data extraction without piping                              #
  # Step 1: Convert sdata to a data frame                       #
  #   sdata <- as.data.frame(sdata)                             #
  # Step 2: Set row names to the values in the 'Clone' column   #
  #   rownames(sdata) <- sdata$Clone                            #  
  # Step 3: Remove the first and second columns using select    #  
  #   sdata <- sdata[, -c(1, 2)]                                #  
  ###############################################################

######### Normal Dendrogram ############

dend <- sdata %>% 
        scale %>% 
        dist %>% 
        hclust %>% 
        as.dendrogram %>%
        set("branches_k_color", k=5) %>% # these are specifications of the plot
        set("branches_lwd", 2.5) %>%
        set("labels_cex", 0.4) 
  #####################################################
  # Apply customizations without piping               #  
  #   dend <- set(dend, "branches_k_color", k = 5)    #
  #   dend <- set(dend, "branches_lwd", 1.5)          #
  #   dend <- set(dend, "labels_cex", 0.4)            #
  #####################################################

plot(dend) + # plot the Dendrogram
  abline(h = 6.5, lty = 2) #this code add line over the plot at specified height h=6.5

""" unique(get_leaves_branches_col(dend)) #this code get the colors in the plot"""

###### Circular Dendrogram #######

hc <- sdata %>% 
      scale %>% 
      dist %>% 
      hclust %>% 
      as.dendrogram() %>% 
      color_branches(k = 5) %>%
      color_labels(k = 5)

  ###############################################
  # Apply color to branches with 5 clusters     #
  #    dend <- color_branches(dend, k = 5)      #
  #                                             #
  # Apply color to labels with 5 clusters       #
  #    dend <- color_labels(dend, k = 5)        #    
  ###############################################

circlize_dendrogram(hc, labels_track_height = NA)
