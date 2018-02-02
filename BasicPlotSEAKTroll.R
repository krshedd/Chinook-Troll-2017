# Copy from SEAKChinookSummary_2004-2017.xlsx

Ewint_P <- read.table('clipboard')
Lwint_P <- read.table('clipboard')
SpringNO_P <- read.table('clipboard')
SpringNI_P <- read.table('clipboard')
SpringSI_P <- read.table('clipboard')
SummerR1_P <- read.table('clipboard')


RGs <- rownames(Ewint_P)

plot_seak.f <- function(df, RGs){
  sapply(RGs[1:7], function(RG) {
    avg <- df[RG, grep(pattern = "Mean", x = colnames(df), value = TRUE)] * 100
    lo = as.numeric(df[RG, grep(pattern = "Lo", x = colnames(df), value = TRUE)] * 100) 
    hi = as.numeric(df[RG, grep(pattern = "Hi", x = colnames(df), value = TRUE)] * 100)
    
    plot(y = avg, x = 2010:2017, xlab = "Year", ylab = paste("Mean %", RG), pch = 16, cex = 3, ylim = c(0, max(hi)))
    arrows(x0 = 2010:2017, x1 = 2010:2017, y0 = lo, y1 =  hi, code = 3, angle = 90, lwd = 3)
  })
}

plot_seak.f(df = Ewint_P, RGs = c("Situk", "Alsek", "NSEAK", "Taku", "Andrew", "Stikine", "SSEAK"))
plot_seak.f(df = Lwint_P, RGs = c("Situk", "Alsek", "NSEAK", "Taku", "Andrew", "Stikine", "SSEAK"))
plot_seak.f(df = SpringNO_P, RGs = c("Situk", "Alsek", "NSEAK", "Taku", "Andrew", "Stikine", "SSEAK"))
plot_seak.f(df = SpringNI_P, RGs = c("Situk", "Alsek", "NSEAK", "Taku", "Andrew", "Stikine", "SSEAK"))
plot_seak.f(df = SpringSI_P, RGs = c("Situk", "Alsek", "NSEAK", "Taku", "Andrew", "Stikine", "SSEAK"))
plot_seak.f(df = SummerR1_P, RGs = c("Situk", "Alsek", "NSEAK", "Taku", "Andrew", "Stikine", "SSEAK"))
