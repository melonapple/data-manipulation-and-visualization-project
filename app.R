#load packages


library(shiny)
library(tidyverse)
library(magrittr)
library(data.table)
library(lubridate)
library(gridExtra)
library(raster)
library(ggmap)

#load file
load('Project.Rdata')

product_sale1=product_sale %>% group_by(시점, 대품목) %>%
  summarise(생산량=sum(생산량, na.rm=T), 생산액=sum(생산액, na.rm=T),
            국내판매량=sum(국내판매량, na.rm=T),
            국내판매액=sum(국내판매액, na.rm=T)) %>%  ungroup() %>% mutate(시점=as.numeric(시점))

# User interface

ui = fluidPage(
  
  titlePanel('커피 및 다류 산업 트랜드 분석'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText('1. 커피 및 다류 산업 현황'),
      
      selectInput('industry', label = '커피, 다류',
                  choices = c('커피', '다류', '커피 및 다류'),
                  selected = '커피'),

      
      selectInput('industry_type', label = '정보',
                  choices = c('수입금액',
                              '생산금액',
                              '국내판매금액',
                              '커피전문점 영업,폐점 수'), 
                  selected = '수입금액'),
      

    ),
    
    mainPanel(plotOutput('plot_industry'))
    
  ),
  
  hr(),
  
  
  fluidRow(
    
    sidebarLayout(
      
      sidebarPanel(
        
        helpText('2. 커피 및 다류 산업 트랜드'),
        
        selectInput('trend', label='정보',
                    choices = c('트랜드 동조성', '품목별 생산량', '품목별 생산금액',
                                           '품목별 판매량', '품목별 판매금액'), 
                      selected = '트랜드 동조성'),
      ),
      
      
      mainPanel(plotOutput('plot_trend'))
      
      
    )
    
  ),
  
  hr(),
  
  fluidPage(
    
    sidebarLayout(
      
      sidebarPanel(
        
        helpText('3. 커피 전문점 산업 현황'),
        
        
        radioButtons('location', label='지역',
                           choices=c('전 지역', '서울'), 
                           selected='전 지역'),
        
        
        selectInput('location_type', label='정보', 
                    choices = c('커피전문점 수', 
                                '커피전문점 신규영업, 폐점 수' 
                                ), 
                    selected='커피전문점 수'),
        
        radioButtons('brand', label='브랜드', 
                           choices=c('전체', '상위 20 브랜드'), 
                           selected='전체'),
        
        numericInput('year', label='년도', 
                  value=2020,
                  )
        
        
        
      ), 
      
      mainPanel(plotOutput('plot_location'))
      
    )
    
  )
  

  
  
  
)




 
# Server logic

server = function(input, output){
  

  
  output$plot_industry = renderPlot(if(input$industry=='커피'){
    
    if (input$industry_type=='수입금액'){
      
      plotting1 = trade_plotting(x=시점, y=수입금액, type='커피', color='brown') + ggtitle('연도별 커피 수입금액') + 
        ylab('수입금액(10000$)') + theme_bw()
      
    
    } else if(input$industry_type=='생산금액') { 
      
      plotting1=ggplot(data=product_sale1 %>% filter(대품목=='커피'), mapping=aes(x=시점, y=생산액)) + 
        geom_line(color='brown') + 
        geom_point(color='brown') + 
        scale_color_manual(values=c('brown')) +
        ggtitle('연도별 커피 생산액') + 
        scale_x_continuous(breaks=2010:2019) + 
        ylab('생산금액(억원)') + 
        theme_bw()
      
    } else if (input$industry_type=='국내판매금액') {
      
      plotting1=ggplot(data=product_sale1 %>% filter(대품목=='커피'), mapping=aes(x=시점, y=국내판매액)) + 
        geom_line(color='brown') + 
        geom_point(color='brown') + 
        scale_color_manual(values=c('brown')) +
        ggtitle('연도별 커피 국내판매액') + 
        scale_x_continuous(breaks=2010:2019) + 
        ylab('판매금액(억원)') + 
        theme_bw()
    } else{
        
      plotting1=p5
      }
    
    return(plotting1)
    
    
  } else if (input$industry=='다류'){
    
    if (input$industry_type=='수입금액'){
      
      plotting1 = trade_plotting(x=시점, y=수입금액, type='차', color='green') + ggtitle('연도별 다류 수입금액') + 
        ylab('수입금액(10000$)') + theme_bw()
      
      
    } else if(input$industry_type=='생산금액') { 
      
      plotting1=ggplot(data=product_sale1 %>% filter(대품목=='다류'), mapping=aes(x=시점, y=생산액)) + 
        geom_line(color='green') + 
        geom_point(color='green') + 
        ggtitle('연도별 다류 생산액') + 
        scale_x_continuous(breaks=2010:2019) + 
        ylab('생산금액(억원)') + 
        theme_bw()
      
    } else if (input$industry_type=='국내판매금액') {
      
      plotting1=ggplot(data=product_sale1 %>% filter(대품목=='다류'), mapping=aes(x=시점, y=국내판매액)) + 
        geom_line(color='green') + 
        geom_point(color='green') + 
        ggtitle('연도별 다류 국내판매액') + 
        scale_x_continuous(breaks=2010:2019) + 
        ylab('판매금액(억원)') + 
        theme_bw()
    } else{
      
      plotting1=p5
    }
    
    return(plotting1)
  } else if (input$industry=='커피 및 다류'){
    
    if (input$industry_type=='수입금액'){
      
      return(grid.arrange(p1, p2, nrow=2))
    } else if (input$industry_type=='생산금액'){
      
      return(p3)
    } else if (input$industry_type=='국내판매금액'){
      return(p4)
    } else {
      return(p5)
    }
    
    
  })
  
  
  output$plot_trend=renderPlot({if (input$trend == '트랜드 동조성'){
    
    return(grid.arrange(survey_trend_result_coffee, survey_trend_result_tea, nrow=1))
    
  } else if (input$trend == '품목별 생산량'){
    
    return(product_sale_1)
    
  } else if (input$trend == '품목별 생산금액'){
    
    return(product_sale_2)
    
  } else if (input$trend == '품목별 판매량'){
    
    return(product_sale_3)
    
  } else if (input$trend == '품목별 판매금액'){
    
    return(product_sale_4)
  }
    
    
    
    })
  
  
  output$plot_location=renderPlot({if (input$location=='전 지역'){
    
    
    if (input$location_type=='커피전문점 수'){
      
      if (input$brand=='전체'){
        
        plotting=year_location_spatialdata(year=as.numeric(input$year)) +
          labs(fill='커피전문점 수', title=paste0(input$year,  '년 지역별 커피전문점 수'))
        return(plotting)
      } else if (input$brand=='상위 20 브랜드'){
        
        plotting=year_location_spatialdata(year=as.numeric(input$year), type = 누적영업top20brand, limit=5000) + 
          labs(fill='상위 브랜드', title=paste0(input$year, '년 상위 브랜드 커피전문점 수'))
        
        return(plotting)
      }
      
    } else if (input$location_type == '커피전문점 신규영업, 폐점 수'){
      
      if (input$brand=='전체'){
        
        return(cafe_new_open_closed)
      } else {
        
        return(cafe_top20_open_closed)
      }
      
    }
    
    
  } else {  
    
    if (input$location_type=='커피전문점 수'){
    
     if (input$brand=='전체'){
      
      plotting=seoul_year_location_spatialdata(year=as.numeric(input$year)) +
        labs(fill='커피전문점 수', title=paste0(input$year,  '년 서울 커피전문점 수'))
      return(plotting)
     } else if (input$brand=='상위 20 브랜드'){
      
      plotting=seoul_year_location_spatialdata(year=as.numeric(input$year), type = 누적영업top20brand, limit=1000) + 
        labs(fill='상위 브랜드', title=paste0(input$year, '년 서울 상위 브랜드 커피전문점 수'))
      
      return(plotting)
      }
    
    } else if (input$location_type == '커피전문점 신규영업, 폐점 수'){
    
      if (input$brand=='전체'){
      
        return(seoul_cafe_new_open_closed)
      } else {
      
      return(seoul_cafe_top20_open_closed)
        
      }
    
  }
}
    
  })
  
  
}



# runapp

shinyApp(ui, server)
