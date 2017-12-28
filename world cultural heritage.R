
library("xlsx")
library("lubridate")
library("ggplot2")
library("plyr")
library("RColorBrewer")
library("dplyr")
library("maptools")
library("ggthemes")
library("leafletCN")
library("leaflet")
library("htmltools")
library("shiny")
library("shinydashboard")
library("rgdal")

mydata<-read.xlsx("E:/Rworkspace/data/File/yichan.xlsx",sheetName="Sheet1",header=T,encoding='UTF-8',stringsAsFactors=FALSE,check.names=FALSE)

mydata$Time<-ymd(mydata$Time)
ggplot(mydata,aes(Time))+
  geom_histogram(binwidth=30)+
  geom_rug()+
  scale_x_date(date_breaks="2 years",date_labels = "%Y")+
  theme_void() %+replace%
  theme(
    axis.text=element_text(),
    plot.margin = unit(c(1,1,1, 1), "lines"),
    axis.line=element_line()
  )

class_count<-plyr::count(mydata$Class)
class_count<-arrange(class_count,freq)
class_count$label_y=c(0,cumsum(class_count$freq)[1:3])+class_count$freq
2class_count$x<-factor(class_count$x,levels=c("世界文化遗产","世界自然遗产","世界文化与自然遗产","世界文化景观遗产"),order=T)
ggplot(class_count,aes(x=1,y=freq,fill=x))+
  geom_col()+
  geom_text(aes(x=1.6,y=label_y,label=paste(round(class_count$freq*100/sum(class_count$freq)),"%")))+
  coord_polar(theta="y")+
  scale_fill_brewer()+
  guides(fill=guide_legend(title=NULL,reverse=T))+
  labs(title="中国世界自然与文化遗产类别占比")+
  theme_void(base_size=15)%+replace%
  theme(plot.margin = unit(c(1,1,1, 1), "lines"))

china_map <- readOGR("E:/Rworkspace/bou2_4p.shp",stringsAsFactors=FALSE)       
ggplot()+ 
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),col="grey60",fill="white",size=.2,alpha=.4)+
  geom_point(data=mydata,aes(x=long,y=lat,shape=Class,fill=Class),size=3,colour="white")+ 
  coord_map("polyconic") +
  scale_shape_manual(values=c(21,22,23,24))+
  scale_fill_wsj()+
  labs(title="中国世界自然文化遗产分布图",caption="数据来源：中国世界遗产名录")+   
  theme_void(base_size=15) %+replace%
  theme(
    plot.title=element_text(size=25,hjust=0),
    plot.caption=element_text(hjust=0),       
    legend.position = c(0.05,0.75),
    plot.margin = unit(c(1,0,1,0), "cm")
  )

for(i in 1:nrow(mydata)){
  mydata$label[i]=sprintf(paste("<b><a href='%s'>%s</a></b>","<p>%s</p>","<p>%s</p>","<p><img src='%s' width='300'></p>",sep="<br/>"),
                          mydata$link[i],mydata$Name[i],mydata$Class[i],mydata$Information[i],mydata$img_link[i])
}
leaflet(china_map)%>%amap()%>%addPolygons(stroke = FALSE)%>%
  addMarkers(data=mydata,lng=~long,lat=~lat,popup=~label)

ui <- dashboardPage(
  dashboardHeader(title = "中国世界遗产名录可视化"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("申请时间与类型分布",     tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("中国世界遗产地域分布",   tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("中国世界遗产分布详情",   tabName = "dashboard3", icon = icon("dashboard")),
      menuItem("中国世界遗产名录摘要",   tabName = "widgets", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "dashboard1",
              fluidRow(
                box(
                  title = "申请时间分布",
                  plotOutput("plot1", height = 500)
                ),
                box(
                  title = "世界遗产类别分布",
                  plotOutput("plot2", height = 500)
                )
              )
      ),
      
      tabItem(tabName = "dashboard2",
              fluidRow(
                box(
                  title = "中国世界遗产地域分布",
                  plotOutput("plot3", width=1000, height=800),
                  width =10
                )
              )
      ),
      
      tabItem(tabName = "dashboard3",
              fluidRow(
                box(
                  title = "中国世界遗产分布详情",
                  leafletOutput("plot4", width = "100%", height = 1000),
                  width =12
                )
              )
      ),
      
      tabItem(tabName = "widgets",
              fluidRow(
                box(
                  title = "中国世界遗产名录摘要",
                  h4("中国作为著名的文明古国，自1985年加入世界遗产公约，至2017年7月，共有52个项目被联合国教科文组织列入《世界遗产名录》，与意大利并列世界第一。其中世界文化遗产32处，世界自然遗产12处，世界文化和自然遗产4处，世界文化景观遗产4处。源远流长的历史使中国继承了一份十分宝贵的世界文化和自然遗产，它们是人类的共同瑰宝。正一艺术最后编辑于2017年7月9日。"),width =12
                )
              )
      )
      
    )
  )
)

server <- shinyServer(function(input, output) { 
  output$plot1 <- renderPlot({
    ggplot(mydata,aes(Time))+
      geom_histogram(binh=30)+
      geom_rug()+
      scale_x_date(date_breaks="2 years",date_labels = "%Y")+
      theme_void() %+replace%
      theme(axis.text=element_text(),plot.margin = unit(c(1,1,1, 1), "lines"),axis.line=element_line())
  })
  output$plot2 <- renderPlot({
    ggplot(class_count,aes(x=1,y=freq,fill=x))+
      geom_col()+
      geom_text(aes(x=1.6,y=label_y,label=paste(round(class_count$freq*100/sum(class_count$freq)),"%")))+
      coord_polar(theta="y")+
      scale_fill_brewer()+
      guides(fill=guide_legend(title=NULL,reverse=T))+
      labs(title="中国世界自然与文化遗产类别占比")+
      theme_void(base_size=15)%+replace%
      theme(plot.margin = unit(c(1,1,1,1), "lines"))
  })
  output$plot3 <- renderPlot({
    ggplot()+ 
      geom_polygon(data=china_map,aes(x=long,y=lat,group=group),col="grey60",fill="white",size=.2,alpha=.4)+
      geom_point(data=mydata,aes(x=long,y=lat,shape=Class,fill=Class),size=3,colour="white")+ 
      coord_map("polyconic") +
      scale_shape_manual(values=c(21,22,23,24))+
      scale_fill_wsj()+
      labs(title="中国世界自然文化遗产分布图",caption="数据来源：中国世界遗产名录")+   
      theme_void(base_size=15) %+replace%
      theme(
        plot.title=element_text(size=25,hjust=0),
        plot.caption=element_text(hjust=0),       
        legend.position = c(0.05,0.75),
        plot.margin = unit(c(1,0,1,0), "cm")
      )
  })
  output$plot4 <- renderLeaflet({
    leaflet(china_map)%>%amap()%>%addPolygons(stroke = FALSE)%>%
      addMarkers(data=mydata,lng=~long,lat=~lat,popup=~label)
  })
})
shinyApp(ui, server)

