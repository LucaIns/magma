mainwd = "YOUR_MAIN_PATH"
setwd(mainwd)

require(ggplot2)
require(ggpubr)
require(egg)
require(tikzDevice)
require(grid)
# options(tikzLatex = "C:\\Users\\USERNAME\\AppData\\Local\\Programs\\MiKTeX\\miktex\\bin\\x64\\pdflatex.exe")


#######################
# Fig. 4
# source("code_matching_graph.R")
#######################

load("data/tmp/fig4.RData")

fsiz= 9
fsizax = 9

leg=unique(res_fin$sim)

p1 = ggplot(data=res_fin, aes(x=Simulated, y=Predicted, group=sim, color=sim)) +
  geom_line(size=lsiz) +
  scale_color_manual(values=cols,
                     labels=leg,
                     name=NULL,
                     breaks=unique(res_fin$sim)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "black", size=lsiz) +
  theme_bw() +
  xlim(1,5) +
  coord_cartesian(xlim=c(1,5), ylim=c(1,5), clip="on") +
  grids(linetype = "dashed") +
  ylab("Predicted Interaction Time [h]") +
  xlab("True Interaction Time [h]") +
  theme(legend.position = c(-0.02, 1.03),
        legend.justification = c(0, 1),
        legend.text = element_text(size=fsiz)) +
  theme(legend.background=element_rect(fill = alpha("white", 0))) +
  theme(aspect.ratio=1,
        axis.text.y = element_text(angle = 90),
        axis.title.x =element_text(vjust=-2),
        axis.title.y =element_text(vjust=4),
        plot.margin = unit(c(1, 1, 2.5, 15), "mm"),
        text = element_text(size = fsizax))
# p1

p2 = ggplot(data=res_fin, aes(x=Simulated, y=abs(Predicted-Simulated), group=sim, color=sim)) +
  geom_line(size=lsiz, show.legend = F) +
  scale_color_manual(values=cols,
                     labels = leg,
                     name=NULL,
                     breaks = unique(res_fin$sim)) +
  theme_bw() +
  xlim(1,5) +
  coord_cartesian(xlim=c(1,5), ylim=c(0,0.8)) +
  grids(linetype = "dashed") +
  xlab("True Interaction Time [h]") +
  ylab("Absolute Error [h]") +
  theme(aspect.ratio=1,
        axis.text.y = element_text(angle = 90),
        axis.title.x =element_text(vjust=-2),
        axis.title.y =element_text(vjust=4),
        plot.margin = unit(c(1, 1, 2.5, 15), "mm"),
        text = element_text(size = fsizax))
# p2

tikz("plots/tex/fig4.tex",
     width = 8, height = 3, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}",
                  "\\usepackage{bm}","\\usepackage{amsthm}","\\usepackage{amsbsy}"
                  ,"\\usepackage{amsbsy}"
                  ,"\\usepackage{amsbsy}"
                  ,"\\usepackage{amsfonts}"
                  ,"\\usepackage{amsmath}"))
p <- egg::ggarrange(p1, p2,
                    labels=c("(a)", "(b)"),
                    label.args = list(gp=gpar(fontface="bold"), hjust=-2),
                    ncol = 2, nrow = 1)
dev.off()


#######################
# Fig. 5
# source("code_matching_graph_subsamples_up_low.R")
#######################

load("data/tmp/fig5.RData")

lsiz = 0.75
fsiz = 10
leg = c("Full sampling", "Sub-sample: 60\\%", "Sub-sample: 30\\%", "Sub-sample: 10\\%")
tit = c("$\\Delta\\rho$ = 20 kg/m$^3$, \\phantom{0}$a$ = 2",
        "$\\Delta\\rho$ = 30 kg/m$^3$, \\phantom{0}$a$ = 1",
        "$\\Delta\\rho$ = 160 kg/m$^3$, $a$ = 0.5")
titsiz = 9
fsizax = 9

pl = list()

for (ind_i in 1:(ind_pl-1)) {

  res_tmp = res_all[[ind_i]]
  ind_y_ax = ind_i == 1 || ind_i == 4 || ind_i == 7
  ind_x_ax = ind_i > 6

  pl[[ind_i]] =
    ggplot(data=res_tmp, aes(x=simulated, y=predicted, group=sub, color=sub)) +
    geom_line(size=lsiz) +
    scale_color_manual(values=cols,
                       labels = leg,
                       name=NULL,
                       breaks = unique(res_tmp$sub)) +
    geom_abline(intercept=0, slope=1, linetype="dashed", color = "black", size=lsiz) +
    theme_bw() +
    grids(linetype = "dashed") +
    theme(axis.title.x =element_text(vjust=-2),
          axis.title.y =element_text(vjust=4)) +
    {if(ind_i<4)
      list(ggtitle(tit[ind_i]),
           theme(plot.title = element_text(hjust = 0.5, size=titsiz)))
    } +
    {if(ind_i>=4)
      list(ggtitle(""),
           theme(plot.title = element_text(hjust = 0.5, size=titsiz)))
    } +
    {if(ind_y_ax)
      list(ylab("Predicted Interaction Time [h]"),
           theme(axis.text.y = element_text(angle = 90),
           plot.margin = unit(c(1, 1, 4, 4), "mm")))
    } +
    {if(!ind_y_ax)
      list(ylab(""),
        theme(plot.margin = unit(c(1, 1, 4, 4), "mm"),
          axis.text.y = element_blank()))
    } +
    {if(ind_x_ax)
      list(xlab("True Interaction Time [h]"),
           theme(plot.margin = unit(c(1, 1, 4, 4), "mm")))
    } +
    {if(!ind_x_ax)
      list(theme(plot.margin = unit(c(1, 1, 4, 4), "mm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank()))
    } +
    {if(ind_i==1)
      theme(legend.position = c(0, 1.065),
            legend.justification = c(0, 1),
            legend.text = element_text(size=fsizax-0.5)) +
        theme(legend.background=element_rect(fill = alpha("white", 0)))
    } +
    {if(ind_i>1)
      theme(legend.position = "none")
    } +
    coord_cartesian(xlim=c(1,5), ylim=c(1,5), clip="on") +
    theme(text = element_text(size = fsizax))
}

tikz("plots/tex/fig5.tex",
     width = 7.5, height = 7.5, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}",
                  "\\usepackage{bm}","\\usepackage{amsthm}","\\usepackage{amsbsy}"
                  ,"\\usepackage{amsbsy}"
                  ,"\\usepackage{amsbsy}"
                  ,"\\usepackage{amsfonts}"
                  ,"\\usepackage{amsmath}"))

p <- egg::ggarrange(pl[[1]], pl[[2]], pl[[3]],
                    pl[[4]], pl[[5]], pl[[6]],
                    pl[[7]], pl[[8]], pl[[9]],
                    labels=paste0("(", letters[1:9], ")"),
                    label.args = list(gp=gpar(fontface="bold"), hjust=-0.5, vjust=1.75),
                    ncol = 3, nrow = 3)

# p
dev.off()

#######################
# Fig. 6
# source("sensitivity_sim.R")
#######################

pll1 = list()
pll2 = list()
pll3 = list()

for (jj in 1:3) {

  simNumber = 2:4
  sim_i = paste0("data/tmp/fig6_sim", simNumber, ".RData")
  load(sim_i[jj])

  col_trans = gg_color_hue(4, alpha = 0.2)
  alphaTrans = 0.2
  lsiz = 1.5
  titsiz = 9
  fsizax = 9

  tit = list()
  tit[[1]] = c("$\\Delta\\rho^*$ (kg/m$^3$) $ \\sim \\mathcal{N}(20, 4)$ \n$a^* \\sim \\mathcal{N}(2, 0.2^2)$",
               "$\\Delta\\rho$ (kg/m$^3$) $ = 20$, $a =2$ \n$\\hat{y}_j^* \\sim \\mathcal{N}(\\hat{y}_j, (\\hat{y}_j/10)^2)$",
               "$\\Delta\\rho^*$ (kg/m$^3$) $\\sim \\mathcal{N}(20, 4)$ \n$a^* \\sim \\mathcal{N}(2, 0.2^2)$, $\\hat{y}_j^* \\sim \\mathcal{N}(\\hat{y}_j, (\\hat{y}_j/10)^2)$")


  tit[[2]] = c("$\\Delta\\rho^*$ (kg/m$^3$) $ \\sim \\mathcal{N}(30, 9)$ \n$a^* \\sim \\mathcal{N}(1, 0.1^2)$",
               "$\\Delta\\rho$ (kg/m$^3$) $ = 30$, $a =1$ \n$\\hat{y}_j^* \\sim \\mathcal{N}(\\hat{y}_j, (\\hat{y}_j/10)^2)$",
               "$\\Delta\\rho^*$ (kg/m$^3$) $\\sim \\mathcal{N}(30, 9)$ \n$a^* \\sim \\mathcal{N}(1, 0.1^2)$, $\\hat{y}_j^* \\sim \\mathcal{N}(\\hat{y}_j, (\\hat{y}_j/10)^2)$")

  tit[[3]] = c("$\\Delta\\rho^*$ (kg/m$^3$) $ \\sim \\mathcal{N}(160, 16^2)$ \n$a^* \\sim \\mathcal{N}(0.5, 0.05^2)$",
               "$\\Delta\\rho$ (kg/m$^3$) $ = 160$, $a =0.5$ \n$\\hat{y}_j^* \\sim \\mathcal{N}(\\hat{y}_j, (\\hat{y}_j/10)^2)$",
               "$\\Delta\\rho^*$ (kg/m$^3$) $\\sim \\mathcal{N}(160, 16^2)$ \n$a^* \\sim \\mathcal{N}(0.5, 0.05^2)$, $\\hat{y}_j^* \\sim \\mathcal{N}(\\hat{y}_j, (\\hat{y}_j/10)^2)$")

  titsiz = 9

  tmp_res = res[[1]]
  names(tmp_res)
  pll1[[jj]] = ggplot(data=tmp_res, aes(x=Simulated, y=Predicted)) +
      geom_line(size=lsiz, colour=cols[1]) +
      geom_ribbon(aes(ymin=CIlow, ymax=CIhigh),
                  alpha=alphaTrans,
                  linetype=1,
                  colour=col_trans[1],
                  size=1,
                  fill=col_trans[1]) +
      scale_color_manual(values=cols,
                         labels = leg,
                         name=NULL) +
      geom_abline(intercept=0, slope=1, linetype="dashed", color = "black", size=lsiz) +
      theme_bw() +
      coord_cartesian(xlim=c(1,5), ylim=c(1,5), clip="on") +
      grids(linetype = "dashed") +
      theme(axis.title.x =element_text(vjust=-2),
            axis.title.y =element_text(vjust=4)) +
      {if(jj==1)
        list(ylab("Predicted Interaction Time [h]"),
             theme(axis.text.y = element_text(angle = 90)))
      } +
      {if(jj>1)
        list(ylab(""),
          theme(axis.text.y = element_blank()))
      } +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank()) +
      ggtitle(tit[[jj]][1]) +
      theme(plot.title = element_text(hjust = 0.5, size=titsiz, vjust=-.5)) +
      theme(legend.position = c(0, 1),
            legend.justification = c(0, 1),
            legend.text = element_text(size=fsiz-2)) +
      theme(legend.background=element_rect(fill = alpha("white", 0))) +
      theme(plot.margin = unit(c(2, 1, 2.5, 2.5), "mm")) +
      theme(text = element_text(size = fsizax))

  tmp_res = res2[[1]]
  names(tmp_res)
  pll2[[jj]] = ggplot(data=tmp_res, aes(x=Simulated, y=Predicted)) +
      geom_line(size=lsiz, colour=cols[1]) +
      geom_ribbon(aes(ymin=CIlow, ymax=CIhigh),
                  alpha=alphaTrans,
                  linetype=1,
                  colour=col_trans[1],
                  size=1,
                  fill=col_trans[1]) +
      scale_color_manual(values=cols,
                         labels = leg,
                         name=NULL) +
      geom_abline(intercept=0, slope=1, linetype="dashed", color = "black", size=lsiz) +
      theme_bw() +
      coord_cartesian(xlim=c(1,5), ylim=c(1,5), clip="on") +
      grids(linetype = "dashed") +
      theme(axis.title.x =element_text(vjust=-2),
            axis.title.y =element_text(vjust=4)) +
      {if(jj==1)
        list(ylab("Predicted Interaction Time [h]"),
             theme(axis.text.y = element_text(angle = 90)))
      } +
      {if(jj>1)
        list(ylab(""),
          theme(axis.text.y = element_blank()))
      } +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank()) +
      ggtitle(tit[[jj]][2]) +
      theme(plot.title = element_text(hjust = 0.5, size=titsiz, vjust=-.5)) +
      theme(legend.position = c(0, 1),
            legend.justification = c(0, 1),
            legend.text = element_text(size=fsiz-2)) +
      theme(legend.background=element_rect(fill = alpha("white", 0))) +
      theme(plot.margin = unit(c(2, 1, 2.5, 2.5), "mm")) +
    theme(text = element_text(size = fsizax))

  tmp_res = res3[[1]]
  names(tmp_res)
  pll3[[jj]] = ggplot(data=tmp_res, aes(x=Simulated, y=Predicted)) +
      geom_line(size=lsiz, colour=cols[1]) +
      geom_ribbon(aes(ymin=CIlow, ymax=CIhigh),
                  alpha=alphaTrans,
                  linetype=1,
                  colour=col_trans[1],
                  size=1,
                  fill=col_trans[1]) +
      scale_color_manual(values=cols,
                         labels = leg,
                         name=NULL) +
      geom_abline(intercept=0, slope=1, linetype="dashed", color = "black", size=lsiz) +
      theme_bw() +
      coord_cartesian(xlim=c(1,5), ylim=c(1,5), clip="on") +
      grids(linetype = "dashed") +
      theme(axis.title.x =element_text(vjust=-2),
            axis.title.y =element_text(vjust=4)) +
      {if(jj==1)
        list(ylab("Predicted Interaction Time [h]"),
             theme(axis.text.y = element_text(angle = 90)))
      } +
      {if(jj>1)
        list(ylab(""),
          theme(axis.text.y = element_blank()))
      } +
      xlab("True Interaction Time [h]") +
      ggtitle(tit[[jj]][3]) +
      theme(plot.title = element_text(hjust = 0.5, size=titsiz, vjust=-.5)) +
      theme(legend.position = c(0, 1),
            legend.justification = c(0, 1),
            legend.text = element_text(size=fsiz-2)) +
      theme(legend.background=element_rect(fill = alpha("white", 0))) +
      theme(plot.margin = unit(c(2, 1, 2.5, 2.5), "mm")) +
    theme(text = element_text(size = fsizax))

}

tikz("plots/tex/fig6.tex",
     width = 7.5, height = 7.5, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}",
                  "\\usepackage{bm}","\\usepackage{amsthm}","\\usepackage{amsbsy}"
                  ,"\\usepackage{mathtools}"
                  ,"\\usepackage{amsbsy}"
                  ,"\\usepackage{amsfonts}"
                  ,"\\usepackage{amsmath}"))
p <- egg::ggarrange(pll1[[1]], pll1[[2]], pll1[[3]],
                    pll2[[1]], pll2[[2]], pll2[[3]],
                    pll3[[1]], pll3[[2]], pll3[[3]],
                    labels=paste0("(", letters[1:9], ")"),
                    label.args = list(gp=gpar(fontface="bold"), hjust=-0.15, vjust=1.75),
                    ncol = 3, nrow = 3)
# p
dev.off()

