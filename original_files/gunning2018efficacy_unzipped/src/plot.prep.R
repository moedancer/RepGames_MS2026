require(ggplot2)
require(RColorBrewer)
## multiple use plotting code

#postscript(fonts='Times')
.fig.fontsize <- 9

.grid.text.size <- 28

#.fam ='Times'
.fam ='sans'
## theme
.gg.theme_bw <- theme_bw(
    base_size=.fig.fontsize+2, base_family=.fam
)

## tables, digits
.tab.digits <- 2

## for maps
## legend in bottom-left, no axes
.gg.theme.noaxes <- theme_bw(base_family=.fam) + theme(
    legend.position='top',
    legend.direction='horizontal',
    legend.margin=margin(1,1,1,1),
    #legend.position=c(0.005,0.005),
    #legend.justification=c(0,0),
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
)

## ggplot margin:
## top, right, bottom, and left 
.gg.plot.margin <- c(-0,3,-3,3) 
.mk.unit <- function(x) unit(x, 'mm')
## red/blue for most
## alternate, blue/green is my3cols[2:3]
my3cols <- brewer.pal(3, 'Set1')
my2cols <- brewer.pal(3, 'Dark2')[1:2]
.scale.2col <- function(.name) scale_color_manual( name=.name, values=my2cols)

## background for ggplot legend
.leg.back <- element_rect(fill='#CCCCCC')

.region.label <- 'Sector: '

## sanitize hash for xtable / latex
## escaping hashes is a paaaaaain.
.san.hash <- function(x) gsub('#', '\\\\#', x)

.tab.size <- 'small'
    
## subset out second day of first city-wide spray
## for plotting?
spray_dates <- subset(.spray_dates, date != as.Date('2014-02-06'))
## fix zone factor levels / order
spray_dates$zone <- mk.revalue.zone(spray_dates$zone)
.disa.dates <- as.Date( c(min='2014-02-01', max='2014-02-25'))
spray_dates <- within(spray_dates, {
    ## use as linetype aesthetic
    disa <- ( date > .disa.dates[1] & date < .disa.dates[2])
})
spray_dates_main <- subset(spray_dates, n_house>50)

## shaded bars of spray dates 
spray_ggplot <- (
    geom_segment(data=spray_dates_main,
        ## y must be in aes for name resolution
        ## but set constant value outside 
        ## to avoid interacting w/scales
        mapping=aes(x=date,xend=date,y=1,yend=0, ymin=0, ymax=0, linetype=disa),
        #linetype=2,
        y=1e12, yend=-1e12, color='#999999', alpha=0.75, size=0.45
    )
) 


spray_dates_exper <- subset(spray_dates_main, !disa)
## as above (copy/paste), different data
spray_ggplot_exper <- (
    geom_segment(data=spray_dates_exper,
        ## y must be in aes for name resolution
        ## but set constant value outside 
        ## to avoid interacting w/scales
        mapping=aes(x=date,xend=date,y=1,yend=0, ymin=0, ymax=0, linetype=disa),
        #linetype=2,
        y=1e12, yend=-1e12, color='#999999', alpha=0.75, size=0.45
    )
) 

## this is getting over-written now...
## add to plot with above to set linetype w/no legend
.spray.linetype <- scale_linetype_manual(guide=F, values=c(2,3))

## YYYY\nMM 
custom.date.scales <- 
    scale_x_date(
        labels=date_format('%Y\n%b'), 
        breaks=date_breaks('months') 
    ) 


## background shaded bars for spray circuits
## requires in plot:
    #+ scale_x_discrete()
    #+ scale_alpha_manual(guide=F, values=.spray.alphas)
.spray.alphas <- c(0,0.075,0.2)
.plot.spray.bars <- (
    ## force discrete x for factors
    ## plot rect first in background, uses numeric x
    geom_rect(stat='identity', aes(
        xmin=circuit2_id-1/2,
        xmax=circuit2_id+1/2,
        alpha=factor(spray_type)
        ), 
        ymin=-100, ymax=100, 
        fill='#000000', color='#ffffff',
        size=0, position='identity'
    )
)


## for hypoth plots (glm results)
## margins for gluing 2 different viewports together
## top is default, nudge bottom down / left
.margin.hypoth.top <- .mk.unit(.gg.plot.margin)
.margin.hypoth.bottom <- .mk.unit(.gg.plot.margin + c(2, 0, 4, -1.0))
## define both, modify bottom
.theme.hypoth.top <- .gg.theme_bw + 
theme(
    legend.direction='horizontal',
    legend.background=.leg.back,
    legend.position=c(0.002, 0.985),
    legend.justification=c(0,1),
    ## clockwise from top
    plot.margin=.mk.unit(.margin.hypoth.top)
)
.theme.hypoth.bottom <- .theme.hypoth.top + theme(
    ## no legend
    plot.margin=.mk.unit(.margin.hypoth.bottom),
    legend.position=c(5,5)
    #axis.text.x = element_text(angle = 90, hjust = 1)
)

## no xaxis labels on top
.theme.no.xlabs <- scale_x_discrete(labels=NULL)
