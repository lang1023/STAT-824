# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
data <- read.csv("segmentation data.csv")
vars <- setdiff(names(data), c("ID", "Income"))
varx <- "Income"
models <- c("kmeans","PCA", "kmeansPolygon")


pageWithSidebar(
    headerPanel('Income Disparity by Demographics'),
    sidebarPanel(
        
        selectInput('xcol', 'X Variable', varx),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        uiOutput('helptext'),
        selectInput('model', 'model', models),
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
        plotOutput('plot1'),
    )
    
)

