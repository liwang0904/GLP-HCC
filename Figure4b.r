library(forestplot)

workdir <- "."
#datafile <- file.path(workdir,"Figure2a_a.csv")
datafile <- file.path(workdir,"Figure2b_a.csv")

data <- read.csv(datafile, stringsAsFactors=FALSE,row.names=NULL)

## Labels defining subgroups are a little indented :)
#subgps <- c(2,3,4,5, 7,8,9,10, 12,13,14,15, 17,18,19,20 ,22,23,24,25, 27,28,29,30, 32,33,34,35, 37,38,39,40, 42,43,44,45)
#subgps <- c(2,3, 5,6, 8,9, 11,12, 14,15, 17,18, 20,21, 23,24, 26,27)
#subgps <- c(3,4,5,8,9,10)

#subgps <- c(3,4,5,6,7,8,11,12,13,14)

#data$case_group[subgps] <- paste("   ",data$case_group[subgps])
#data$blah[subgps] <- paste("      ",data$blah[subgps])

## Combine the count and percent column
np <- ifelse(!is.na(data$case_group), paste(data$case_group," (",data$p_val,")",sep=""), NA)

## The rest of the columns in the table. 
tabletext <- cbind(c("Cohort size",data$casecount),
                   c("Exposure\n cohort\n",data$controlcount),
                   c("Comparison\n cohort\n",data$blah),
                   c("# of cases (overall risk)\n(Exposure cohort)\n",data$case_group),
                   c("# of cases (overall risk)\n(Comparison cohort)\n",data$control_group),
                   
                   c("        HR (95% CI)",data$CI)
)

#pdf(file.path(workdir,"Figure2a_a.pdf"),  onefile=FALSE, width=16, height=7)
pdf(file.path(workdir,"Figure2b_a.pdf"),  onefile=FALSE, width=16, height=7)

forestplot(labeltext=tabletext, 
           graphwidth = unit(90, 'mm'),
           graph.pos=6,
          # is.summary=c(TRUE,TRUE, rep(FALSE, 6),TRUE,rep(FALSE, 6),TRUE,rep(FALSE, 5),TRUE,rep(FALSE, 8)),
           #is.summary=c(TRUE,TRUE, rep(FALSE, 6),TRUE,rep(FALSE, 6),TRUE,rep(FALSE, 5),TRUE,rep(FALSE, 8)),
          is.summary=c(TRUE,TRUE,rep(FALSE, 6), TRUE,rep(FALSE,10)),
          
          # is.summary=c(TRUE,TRUE,TRUE,rep(FALSE,6),TRUE,rep(FALSE,10)),
           #is.summary=c(TRUE,TRUE,TRUE,rep(FALSE,4),TRUE,rep(FALSE,4),TRUE,rep(FALSE,4),TRUE,rep(FALSE,4),TRUE,rep(FALSE,4),TRUE,rep(FALSE,4),TRUE,rep(FALSE,4),TRUE,rep(FALSE,4),TRUE,rep(FALSE,10)),
           #is.summary=c(TRUE,TRUE,TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,2),TRUE,rep(FALSE,10)),
           #is.summary=c(TRUE,rep(FALSE,10)),
           mean=c(NA,data$RR), 
           lower=c(NA,data$down), upper=c(NA,data$up),
          
          #title ="Incident hepatocellular carcinoma in patients with type 2 diabetes\n with and without alcohol or tobacco use disorders \n(Comparison between propensity-score matched cohorts)", 
          title ="Hepatic decompensating events in patients with type 2 diabetes\n with and without alcohol or tobacco use disorders \n(Comparison between propensity-score matched cohorts)", 
         
           #xticks = seq(exp(-0.26),exp(1.65),exp(0.06)), ###range of x axis
           #xticks = seq(0,2.25,0.25),
           xticks=log(c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,3,4,5)),
           
           #xticks=10^(seq(-1,1,0.1)),
           xlog = TRUE,
            xlab="Hazard Ratio (HR)", ##name of x axis

           #### Add horizontal lines on the plot
           hrzl_lines=list("2" = gpar(lwd = 1.2, lty=1, col='black'),
                          "3" = gpar(lwd = 0.9, lty='longdash', col='black'),
                          "10" = gpar(lwd = 0.9, lty='longdash', col='black')
                         # "15" = gpar(lwd = 0.9, lty='longdash', col='black'),
                         # "21" = gpar(lwd = 0.9, lty='longdash', col='black')
                          #"10" = gpar(lwd = 0.9, lty='longdash', col='black'),
                          #"13" = gpar(lwd = 0.9, lty='longdash', col='black')
                          #"16" = gpar(lwd = 0.9, lty='longdash', col='black')
                           #"5" = gpar(lwd = 0.9, lty='longdash', col='black')
                          #"6" = gpar(lwd = 0.7, lty='longdash', col='black'),
                          #"10" = gpar(lwd = 0.9, lty=1, col='black'),
                          #"11" = gpar(lwd = 0.7, lty='longdash', col='black')
                           #"11" = gpar(lwd = 0.7, lty=5, col='black')
                           
           ),
          
          #txt_gp=fpTxtGp(label=list(gpar(cex=1.5,fontface='bold'),gpar(cex=1.5),gpar(cex=1.5),gpar(cex=1.5),gpar(cex=1.5),gpar(cex=1.5),gpar(cex=1.5)),
          txt_gp=fpTxtGp(label=list(gpar(cex=0.9),gpar(cex=0.9),gpar(cex=0.9),gpar(cex=0.9),gpar(cex=0.9),gpar(cex=0.9)),
                         ticks=gpar(cex=0.9),
                         xlab=gpar(cex=0.9,col='black',fontface='bold'),
                         #xlab=gpar(cex = 1.8),
                         title=gpar(cex = 1.3)),
          #clip=c(0.001, 4.5), 
          
          col=fpColors(box="black", lines="black", zero = "black"),
          lwd.zero = 001,
          lwd.xaxis = 0.7,
          mar = unit(c(0,0,0,0), "mm"),
          zero=1, cex=0.01, lineheight = unit(2, "mm"), boxsize=0.2, colgap=unit(3,"mm"),
          lwd.ci=0.8, ci.vertices=TRUE, ci.vertices.height = 0.15)

dev.off()

