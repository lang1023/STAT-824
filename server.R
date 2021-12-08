

function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        data[, c(input$xcol, input$ycol)]
    })
    
    
    clusters <- reactive({
        if(input$model == 'kmeans'){
        kmeans(selectedData(), input$clusters)
        } else if (input$model == 'PCA'){
            library(FactoMineR)
            library(factoextra)
            res.PCA<-PCA(selectedData(),graph=FALSE)
            fviz_eig(res.PCA)
            fviz_pca_ind(res.PCA,repel=TRUE)
            plotPCA123 <- fviz_pca_biplot(res.PCA,repel=TRUE)
        } else if (input$model == 'hclust') {
            library(tidyr)
            res.hc <- selectedData() %>%
                scale() %>%
                eclust("hclust", k = input$clusters, graph = FALSE)
        } else if (input$model == 'kmeansPolygon') {
            km.res <- kmeans(selectedData(), input$clusters, nstart = 1)
            library(factoextra)
        }
    })
    
    output$helptext <- renderUI({
        switch(input$ycol,
               Sex = helpText('Biological sex (gender) In this dataset there are only 2 different options. 0 = Male | 1 = female'),
               Age = helpText('The age of the person in years. Min = 18 | Max = 76'),
               Marital.status = ('Marital status of a person 0 = Single | 1 = non-single (divorced / separated / married / widowed)'),
               Education = ('Level of education of the person 0 = other/unknown | 1 = high school | 2 = University | 3 = graduate school'),
               Occupation = ('Category of occupation of the person 0 = unskilled | 1 = skilled | 2 = management/highly skilled/self-employed'),
               Settlement.size = ('The size of the city that the person lives in. 0 = Small City | 1 = mid-sized City | 2 = large city')
              
        )
    })
    
    output$plot1 <- renderPlot({
        if(input$model == 'kmeans'){
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20
             #,cex = 3
             )
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
        } else if (input$model == 'PCA'){
            library(FactoMineR)
            library(factoextra)

            res.PCA<-PCA(selectedData(),graph=FALSE)
            fviz_pca_biplot(res.PCA,repel=TRUE,col.ind = data$ID, 
                            palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                        "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
                            ,label = FALSE)
        } else if (input$model == 'hclust') {
            library(tidyr)
            library(factoextra)
            res.hc <- selectedData() %>%
                scale() %>%
                eclust("hclust", k = input$clusters, graph = FALSE)
            
            # Visualize with factoextra
            fviz_dend(res.hc, palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),
                      rect = TRUE, show_labels = FALSE)
        } else if(input$model == 'kmeansPolygon'){
            km.res <- kmeans(selectedData(), input$clusters, nstart = 1)
            # Visualize

            library(factoextra)
            fviz_cluster(km.res, data = selectedData(),
                         ellipse.type = "convex",
                         palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                     "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),
                         ggtheme = theme_minimal())
        }
    })
    
}



