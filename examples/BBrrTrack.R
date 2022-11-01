library(fieldMapR)
load("yieldTrials_2022_BLAVA.RData")

names(trials)
plotCex <- 0.5
makePDF <- TRUE
dir <- "maps/"
loc <- "BLAVA"
year <- 2022
fieldName  <- "RailroadTrack"
trialColors <- c(WinterMalt = 1, 
				 EasternMalt = 2,
				 AckermannMalt = 7,
				 StateBarley = 9,
				 HulledAdvance = 3,
				 HulledPrelim = 10,
				 HulledObs = 11,
				 UniformBarley = 12,
				 HullessElite = 13,
				 HullessPrelim = 14,
				 HullessObs = 15,
				 SRWObs = 21,
				 UniformScab = 1,
				 JoMarA = 2,
				 JoMarB = 3,
				 Jessica = 22,
				 StateWheat = 16,
				 SRWPrelim = 17,
				 MasonDixon = 19,
				 GulfAtlantic = 23,
				 UniformSouthern = 20,
				 UniformEastern = 30,
				 WhiteWheat = 24,
				 HRWElite = 26,
				 HRWObs = 25,
				 HRWPrelim = 27,
				 UniformHRW = 28,
				 HullessPartial = 29,
				 HullessExtra = 30,
				 breederSeed = 31)




plotsPerTrial <- sapply(trials, function(x) length(x@plotNo))
print(plotsPerTrial)
sum(plotsPerTrial)

cols <- cols30
alpha <- "B3" # 70
colsAlpha <- paste0(cols, alpha)
# 
reorder <- c(13, 6, 5, 7, 4, 12, 11, 3, 8, 2, 9, 1, 10)
boundries <- readGoogleGPS("~/Dropbox/fieldMapR/rrtrackField.csv", reorder)

# check order, and define reorder above. 
checkOrder <- FALSE
if(checkOrder){
	plot(boundries, pch = NA)
	text(boundries, labels = 1:nrow(boundries))
}

boundryFt <- GPStoDist(boundries)

whichAngle <- 2

rotateWholeMap <- -boundryFt@angle[[whichAngle]]
rrtrack <- rotateMap(boundryFt, rotateWholeMap)
# plot(rrtrack$points, type = "n", asp = 1)

sqFtPerAcre <- 43560
rrtrackAcres <- polygonArea(boundryFt@points) / sqFtPerAcre
print(rrtrackAcres)

a <- rrtrack@angle[[whichAngle]] 
refCorner <- rrtrack@points[1,]

inx <- 5
iny <- 5
startPt <- shiftPt(refCorner, a, x = inx, y = iny)


xlims <- c(0, 1500)
ylims <-  c(0, 400)
checkField <- FALSE
if (checkField){
	showBno <- FALSE
	plot(rrtrack$points, type = "n", asp = 1, xlim = xlims, ylim = ylims)
	lines(rrtrack$points)
	text(rrtrack$points, labels = 1:{nrow(rrtrack$points)-1})
	points(startPt[[1]], startPt[[2]], col = "red", pch = 16)
} else {
	showBno <- TRUE
}

rrRanges1 <- 20

sw <- trials[["StateWheat_2022_BLAVA"]]

state12 <- makePlots(trial = trials[["StateWheat_2022_BLAVA"]][[1:2]], ranges = rrRanges1, passes = 15, ref = startPt, angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(1,0), borderName = "W", fill = TRUE)

state34 <- makePlots(trial = trials[["StateWheat_2022_BLAVA"]][[3:4]], ranges = rrRanges1, passes = 15, ref = getCorner(state12), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(1,0), borderName = "W", fill = TRUE)
# l <- list(state12, state34)
StateWheat <- cbind(state12, state34)
# drawPlots(StateWheat, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["StateWheat"]], "gray"), cex = plotCex, plotNos = showBno)

prePass <- 20

SRWPrelim <- makePlots(trial = trials[["SRWPrelim_2022_BLAVA"]], ranges = rrRanges1, 
				   passes = prePass, ref = getCorner(StateWheat), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(1,0), borderName = "W", fill = FALSE)
# drawPlots(SRWPrelim, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["SRWPrelim"]], "gray"), cex = plotCex, plotNos = showBno)
length(SRWPrelim)

UniformSouthern <- makePlots(trial = trials[["UniformSouthern_2022_BLAVA"]], ranges = rrRanges1, 
				   passes = prePass, ref = getCorner(StateWheat), angle = a, 
				   pstart = P, rstart = R, 
				   border = c(1,0), borderName = "W", fill = TRUE)
# drawPlots(UniformSouthern, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["UniformSouthern"]], "gray"), cex = plotCex, plotNos = showBno)

Jessica <- makePlots(trial = trials[["Jessica_2022_BLAVA"]], ranges = rrRanges1, 
				   passes = 35, ref = getCorner(SRWPrelim), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(1,1), borderName = "W", fill = TRUE)
# drawPlots(Jessica, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["Jessica"]], "gray"), cex = plotCex, plotNos = showBno)

MasonDixon <- makePlots(trial = trials[["MasonDixon_2022_BLAVA"]], ranges = rrRanges1, 
				   passes = 36, ref = getCorner(SRWPrelim), angle = a, 
				   pstart = 1, rstart = R, ignorePasses = c(22:37),
				   border = c(1,0), borderName = "W", fill = FALSE)
# drawPlots(MasonDixon, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["MasonDixon"]], "gray"), cex = plotCex, plotNos = showBno)

fillPlots <- paste0("W", B)
mdxnfill <- makePlots(trial = "mdxnfill", ranges = rrRanges1, 
				   passes = 36, ref = getCorner(SRWPrelim), angle = a, 
				   pstart = P, rstart = R, 
				   plotNo = fillPlots, border = c(1,0), borderName = "W", updateB = FALSE, fill = FALSE)
# drawPlots(mdxnfill, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["MasonDixon"]], "gray"), cex = plotCex, plotNos = showBno)
B <- B + 1
MasonDixon <- combinePlots(MasonDixon, mdxnfill)

KWSS01 <- makePlots(trial = trials[["KWSS01_2022_BLAVA"]], ranges = rrRanges1, 
				   passes = 36, ref = getCorner(SRWPrelim), angle = a, 
				   pstart = 21, rstart = 11, ignorePasses = c(1:21),
				   border = c(1,0), borderName = "W", fill = TRUE)
# drawPlots(KWSS01, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["KWSS01"]], "gray"), cex = plotCex, plotNos = showBno)

UniformEastern <- makePlots(trial = trials[["UniformEastern_2022_BLAVA"]], ranges = rrRanges1, 
				   passes = 36, ref = getCorner(SRWPrelim), angle = a, 
				   pstart = 6, rstart = 19, #ignorePasses = 37,
				   border = c(1,0), borderName = "W", fill = TRUE)
# drawPlots(UniformEastern, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["UniformEastern"]], "gray"), cex = plotCex, plotNos = showBno)

SRWObs <- makePlots(trial = trials[["SRWObs_2022_BLAVA"]], ranges = rrRanges1, 
				   passes = 35, ref = getCorner(Jessica), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(1,1), borderName = "W", fill = TRUE)
# drawPlots(SRWObs, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["SRWObs"]], "gray"), cex = plotCex, plotNos = showBno)

rrRanges2 <- 19

HRWObs <- makePlots(trial = trials[["HRWObs_2022_BLAVA"]], ranges = rrRanges2, 
				   passes = 10, ref = getCorner(SRWObs), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(0,1), borderName = "W", fill = FALSE)
# drawPlots(HRWObs, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HRWObs"]], "gray"), cex = plotCex, plotNos = showBno)


WhiteWheat <- makePlots(trial = trials[["WhiteWheat_2022_BLAVA"]], ranges = rrRanges2, 
				   passes = 10, ref = getCorner(SRWObs), angle = a, 
				   pstart = P, rstart = R, blockSize = 7, nBlock = 2,
				   border = c(0,1), borderName = "W", fill = FALSE)
# drawPlots(WhiteWheat, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["WhiteWheat"]], "gray"), cex = plotCex, plotNos = showBno)


HRWElite <- makePlots(trial = trials[["HRWElite_2022_BLAVA"]], ranges = rrRanges2, 
				   passes = 24, ref = getCorner(HRWObs), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(0,1), borderName = "W", fill = FALSE)
# drawPlots(HRWElite, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HRWElite"]], "gray"), cex = plotCex, plotNos = showBno)



HRWPrelim <- makePlots(trial = trials[["HRWPrelim_2022_BLAVA"]], ranges = rrRanges2, 
				   passes = 24, ref = getCorner(HRWObs), angle = a, 
				   pstart = P, rstart = R, 
				   border = c(0,1), borderName = "W", fill = FALSE)
# drawPlots(HRWPrelim, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HRWPrelim"]], "gray"), cex = plotCex, plotNos = showBno)


UniformHRW <- makePlots(trial = trials[["UniformHRW_2022_BLAVA"]], ranges = rrRanges2, 
				   passes = 24, ref = getCorner(HRWObs), angle = a, 
				   pstart = P, rstart = R, 
				   border = c(0,1), borderName = "W", fill = TRUE)
# drawPlots(UniformHRW, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["UniformHRW"]], "gray"), cex = plotCex, plotNos = showBno)

barley <- plotsPerTrial[grep("Malt|Hull|Barley", names(plotsPerTrial))]
sum(barley)
rrRanges3 <- 18

sum(plotsPerTrial[c("AckermannMalt_2022_BLAVA", "EasternMalt_2022_BLAVA", "WinterMalt_2022_BLAVA")])

rrRanges4 <- 17

hullessNo <- sum(plotsPerTrial[c("HullessElite_2022_BLAVA", "HullessObs_2022_BLAVA", "HullessPrelim_2022_BLAVA")])
hullessNo / 17

hullessStart <- shiftPt(getCorner(HRWElite), x = 0, a = a)

HullessPrelim <- makePlots(trial = trials[["HullessPrelim_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 14, ref = hullessStart, angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(1,1), borderName = "H", fill = FALSE)
# drawPlots(HullessPrelim, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HullessPrelim"]], "gray"), cex = plotCex, plotNos = showBno)

HullessObs <- makePlots(trial = trials[["HullessObs_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 14, ref = hullessStart, angle = a, 
				   pstart = P, rstart = R, 
				   border = c(1,1), borderName = "H", fill = TRUE)
# drawPlots(HullessObs, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HullessObs"]], "gray"), cex = plotCex, plotNos = showBno)

HullessElite <- makePlots(trial = trials[["HullessElite_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 14, ref = hullessStart, angle = a, 
				   pstart = P, rstart = R, 
				   border = c(1,1), borderName = "H", fill = TRUE)
# drawPlots(HullessElite, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HullessElite"]], "gray"), cex = plotCex, plotNos = showBno)



StateBarley <- makePlots(trial = trials[["StateBarley_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 14, ref = getCorner(HullessPrelim), angle = a, 
				   blockSize = 41, nBlock = 4,
				   pstart = 1, rstart = 1, 
				   border = c(1,1), borderName = "B", fill = FALSE)
# drawPlots(StateBarley, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["StateBarley"]], "gray"), cex = plotCex, plotNos = showBno)


UniformBarley <- makePlots(trial = trials[["UniformBarley_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 14, ref = getCorner(HullessPrelim), angle = a, 
				   pstart = P, rstart = R, 
				   border = c(1,1), borderName = "B", fill = TRUE)
# drawPlots(UniformBarley, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["UniformBarley"]], "gray"), cex = plotCex, plotNos = showBno)



hulled <- barley[grep("Hulled|Malt|Barley", names(barley))]

17*18

164 + sum(hulled)

hulledStart <- shiftPt(getCorner(StateBarley), x = 30, a = a)


HulledObs <- makePlots(trial = trials[["HulledObs_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 23, ref = hulledStart, angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(1,1), borderName = "M", fill = TRUE)
# drawPlots(HulledObs, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HulledObs"]], "gray"), cex = plotCex, plotNos = showBno)



HulledAdvance <- makePlots(trial = trials[["HulledAdvance_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 19, ref = getCorner(HulledObs), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(0,1), borderName = "M", fill = TRUE)
# drawPlots(HulledAdvance, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HulledAdvance"]], "gray"), cex = plotCex, plotNos = showBno)

HulledPrelim <- makePlots(trial = trials[["HulledPrelim_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 19, ref = getCorner(HulledObs), angle = a, 
				   pstart = P, rstart = R, 
				   border = c(0,1), borderName = "M", fill = FALSE)
# drawPlots(HulledPrelim, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HulledPrelim"]], "gray"), cex = plotCex, plotNos = showBno)



WinterMalt <- makePlots(trial = trials[["WinterMalt_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 19, ref = getCorner(HulledObs), angle = a, 
				   pstart = P, rstart = R, 
				   border = c(0,1), borderName = "M", fill = TRUE)
# drawPlots(WinterMalt, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["WinterMalt"]], "gray"), cex = plotCex, plotNos = showBno)

AckermannMalt <- makePlots(trial = trials[["AckermannMalt_2022_BLAVA"]], ranges = rrRanges4, 
				   passes = 19, ref = getCorner(HulledObs), angle = a, 
				   pstart = P, rstart = R, ignorePasses = 19, 
				   border = c(0,1), borderName = "M", fill = TRUE)
# drawPlots(AckermannMalt, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["AckermannMalt"]], "gray"), cex = plotCex, plotNos = showBno)


fillPlots <- paste0("M", {B}:{B+1})
Ackerfill <- makePlots(trial = "mdxnfill", ranges = rrRanges4, 
				   passes = 19, ref = getCorner(HulledObs), angle = a, 
				   pstart = 19, rstart = 16, ignorePasses = c(1:18),
				   plotNo = fillPlots, border = c(0,1), borderPlotNo = 2, borderName = "M", updateB = FALSE, fill = FALSE, )
# drawPlots(mdxnfill, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["MasonDixon"]], "gray"), cex = plotCex, plotNos = showBno)
B <- B + length(fillPlots) + 1

# this is really hacky. Need to fix so that you can have !!!
Ackerfill@centers <- Ackerfill@centers[1:2]
Ackerfill@corners <- Ackerfill@corners[1:2]
Ackerfill@matrix[1,20] <- NA
AckermannMalt <- combinePlots(AckermannMalt, Ackerfill)

EasternMalt <- makePlots(trial = trials[["EasternMalt_2022_BLAVA"]], ranges = 9, 
				   passes = 14, ref = getCorner(HulledAdvance), angle = a, 
				   pstart = 1, rstart = 1, 
				   border = c(0,1), borderName = "M", fill = TRUE)
# drawPlots(HulledObs, angle = a, labAngle = a + 90, color = c(colsAlpha[trialColors["HulledObs"]], "gray"), cex = plotCex, plotNos = showBno)



BShrStart <- shiftPt(getCorner(EasternMalt), x = 18, y = 0)
# headrows <- makePlots(trial = "headrows", ranges = 1, rangeDist = 152, 
# 				   passes = 4, ref = BShrStart, angle = a, 
# 				   pstart = 1, rstart = 1,
# 				   plotNo = paste0(1:26),
# 				   border = c(0,1), borderName = "B", fill = TRUE)



bseed <- read.csv("breederSeedTraysBB.csv")
bseed <- bseed[bseed$field == "rrTrack",]

bseedLong <- data.frame(Type = rep(bseed$Type, times = bseed$trays), Line = rep(bseed$Line, times = bseed$trays), tray = unlist(lapply(bseed$trays, function(x) 1:x)))
bseedLong$trayName <- paste0(bseedLong$Line, " T-", bseedLong$tray)
bseedLong$col <- colsAlpha[trialColors["breederSeed"]]
longB <- bseedLong[1,]
longB$Line <- "fill"
longB$tray <- 1
longB$trayName <- "Barley" 
longB$col <- rgb(t(col2rgb("grey")) / 255)
cnt <- 1
for(i in unique(bseedLong$Line)[1:5]){
	longBi <- longB
	longBi$tray <- cnt
	longBi$trayName <- paste0("Barley T-", cnt)
	isLine <- which(bseedLong$Line == i)

	if(max(isLine) < nrow(bseedLong)) {
		bseedLong <- rbind(bseedLong[1:max(isLine), ], longBi, bseedLong[{max(isLine)+1}:nrow(bseedLong), ])
	} else {
		# bseedLong <- rbind(bseedLong[1:max(isLine), ], longBi)
	}
	cnt <- cnt + 1
}
hrAng = 0

BS_14VDH_SRW14_150 <- makePlots(trial = "14VDH-SRW14-150", ranges = 1, rangeDist = 152, 
				   passes = 4, ref = BShrStart, angle = hrAng, 
				   pstart = 1, rstart = 1,
				   plotNo = paste0("14VDH-SRW14-150 T-", 1:4),
				   border = c(0,1), borderName = "B", fill = TRUE)


BS_17VDH_SRW05_170 <- makePlots(trial = "17VDH-SRW05-170", ranges = 1, rangeDist = 152, 
				   passes = 4, ref = getCorner(BS_14VDH_SRW14_150), angle = hrAng, 
				   pstart = 1, rstart = 1,
				   plotNo = paste0("17VDH-SRW05-170 T-", 1:4),
				   border = c(0,1), borderName = "B", fill = TRUE)


BS_DH16_SRW120_064 <- makePlots(trial = "DH16-SRW120-064", ranges = 1, rangeDist = 152, 
				   passes = 4, ref = getCorner(BS_17VDH_SRW05_170), angle = hrAng, 
				   pstart = 1, rstart = 1,
				   plotNo = paste0("DH16-SRW120-064 T-", 1:4),
				   border = c(0,1), borderName = "B", fill = TRUE)


BS_VA20W_69 <- makePlots(trial = "VA20W-69", ranges = 1, rangeDist = 152, 
				   passes = 4, ref = getCorner(BS_DH16_SRW120_064), angle = hrAng, 
				   pstart = 1, rstart = 1,
				   plotNo = paste0("VA20W-69 T-", 1:4),
				   border = c(0,1), borderName = "B", fill = TRUE)

BS_VA20W_171 <- makePlots(trial = "VA20W-171", ranges = 1, rangeDist = 152, 
				   passes = 4, ref = getCorner(BS_VA20W_69), angle = hrAng, 
				   pstart = 1, rstart = 1,
				   plotNo = paste0("VA20W-171 T-", 1:4),
				   border = c(0,1), borderName = "B", fill = TRUE)

BS_VA20W_171 <- makePlots(trial = "VA20W-171", ranges = 1, rangeDist = 152, 
				   passes = 3, ref = getCorner(BS_VA20W_69), angle = hrAng, 
				   pstart = 1, rstart = 1,
				   plotNo = paste0("VA20W-171 T-", 1:4),
				   border = c(0,1), borderName = "B", fill = TRUE)

BS_Hilliard <- makePlots(trial = "Hilliard", ranges = 1, rangeDist = 152, 
				   passes = 2, ref = getCorner(BS_VA20W_171), angle = hrAng, 
				   pstart = 1, rstart = 1,
				   plotNo = paste0("Hilliard T-", 1:4),
				   border = c(0,0), borderName = "B", fill = TRUE)

breederSeed <- cbind(BS_14VDH_SRW14_150, BS_17VDH_SRW05_170, BS_DH16_SRW120_064, BS_VA20W_69, BS_VA20W_171, BS_Hilliard)
breederSeed@trialName <- "breederSeed"

print(c(breederSeed@matrix))
length(breederSeed)
length(bseedLong$col)


trialPlots <- list(StateWheat = StateWheat,
					SRWPrelim = SRWPrelim,
					UniformSouthern = UniformSouthern,
					Jessica = Jessica,
					MasonDixon = MasonDixon,
					KWSS01 = KWSS01,
					UniformEastern = UniformEastern,
					SRWObs = SRWObs,
					HRWObs = HRWObs,
					WhiteWheat = WhiteWheat,
					HRWElite = HRWElite,
					HRWPrelim = HRWPrelim,
					UniformHRW = UniformHRW,
					HullessPrelim = HullessPrelim,
					HullessObs = HullessObs,
					HullessElite = HullessElite,
					StateBarley = StateBarley,
					UniformBarley = UniformBarley,
					HulledObs = HulledObs,
					HulledAdvance = HulledAdvance,
					HulledPrelim = HulledPrelim,
					WinterMalt = WinterMalt,
					AckermannMalt = AckermannMalt,
					EasternMalt = EasternMalt,
					breederSeed = breederSeed
)


centers <- do.call(rbind, lapply(trialPlots, getCenter))


makeBlocks <- TRUE
if(makeBlocks){

	blocks <- list(rrTrackI = makeBlock(trialPlots[c("StateWheat")], blockName = "rrTrackI"),
				   rrTrackII = makeBlock(trialPlots[c("SRWPrelim", "UniformSouthern")], blockName = "rrTrackII"),
				   rrTrackIII = makeBlock(trialPlots[c("Jessica", "MasonDixon", "KWSS01", "UniformEastern")], blockName = "rrTrackIII"),
				   rrTrackIV = makeBlock(trialPlots[c("SRWObs")], blockName = "rrTrackIV"),
				   rrTrackV = makeBlock(trialPlots[c("HRWObs", "WhiteWheat")], blockName = "rrTrackV"),
				   rrTrackVI = makeBlock(trialPlots[c("HRWElite", "HRWPrelim", "UniformHRW")], blockName = "rrTrackVI"),
				   rrTrackVII = makeBlock(trialPlots[c("HullessPrelim", "HullessObs", "HullessElite")], blockName = "rrTrackVII"),
				   rrTrackVIII = makeBlock(trialPlots[c("StateBarley", "UniformBarley")], "rrTrackVIII"),
				   rrTrackIX = makeBlock(trialPlots[c("HulledObs")], blockName = "rrTrackIX"),
				   rrTrackX = makeBlock(trialPlots[c("HulledAdvance", "HulledPrelim", "WinterMalt", "AckermannMalt")], "rrTrackX"), 
				   rrTrackXI = makeBlock(trialPlots[c("EasternMalt")], "rrTrackXI")
				   ) 

	# fix planting errors
	wheatFix <- list(c("SRWObs_2022_BLAVA_1217", "SRWObs_2022_BLAVA_1134"), c("SRWObs_2022_BLAVA_1134", "SRWObs_2022_BLAVA_1217"))
	
	barfix <- list(c("HulledObs_2022_BLAVA_38", "HulledObs_2022_BLAVA_55"), c("HulledObs_2022_BLAVA_55", "HulledObs_2022_BLAVA_38"))

	blocks[["rrTrackIV"]] <- fixPlantingErrors(blocks[["rrTrackIV"]], swap = wheatFix)
	blocks[["rrTrackIX"]] <- fixPlantingErrors(blocks[["rrTrackIX"]], swap = barfix)

	# remove second border pass for Jessicas block so that last row of KWS is harvested
	blocks[["rrTrackIII"]]@borderPasses <- blocks[["rrTrackIII"]]@borderPasses[1]

	save(blocks, file = paste0("metaData/", fieldName, "_", year, "_", loc, ".RData"))

	cumPass <- FALSE
	lastpass <- 0
	for(i in blocks){
		writeBags(i, dir = "bags/", addPass = lastpass, countBorder = cumPass)
		writeStakes(i, dir = "stakes/", addPass = lastpass)
		if(cumPass) lastpass <- lastpass + max(i@long[["pass"]])
	}

	cumPass <- TRUE
	lastpass <- 0
	for(i in blocks){
		writeBags(i, dir = "bagsCumPass/", addPass = lastpass, countBorder = cumPass)
		writeStakes(i, dir = "stakesCumPass/", addPass = lastpass)
		if(cumPass) lastpass <- lastpass + max(i@long[["pass"]])
		# NEED TO FIX writeHarvMasterFile to include fill plots (if necessary?). Also need to add Line and Ped info
		# consider adding fillPlots indices as a slot in fieldPlots
		writeHarvMasterFile(i, dir = "harvMaster/", rmBorderRanges = TRUE) 
	}


}


sum(sapply(blocks, length))
sum(sapply(blocks[1:6], length))
# 0.001836547 acres per plot
0.001836547 * sum(sapply(blocks[1:6], length))


if(makePDF) pdf(paste0(dir, "fieldMapRailroadTrackBB.pdf"), width = 36, height = 24)

plotField(rrtrack, xlim = c(0, 1500))

headrows <- c("breederSeed")
for(i in names(trialPlots)){
	if(!i %in% headrows) drawPlots(trialPlots[[i]], angle = a, labAngle = a + 90, rtrim = 3, color = c(colsAlpha[trialColors[i]], "gray"), cex = plotCex, plotNos = showBno)
}

drawPlots(breederSeed, rtrim = c(0, 15), angle = a, labAngle = a + 90, color = bseedLong$col, cex = plotCex, plotNos = showBno, showBorderNum = TRUE)


calcDist(getCorner(StateWheat, "bottomleft"), getCorner(StateWheat, "topleft"), offset = c(40, 44))
calcDist(getCorner(StateWheat, "bottomleft"), getCorner(StateBarley, "bottomright"), offset = c(-20, -24))

calcDist(getCorner(StateWheat, "topleft"), getCorner(SRWObs, "topright"), offset = c(50, 54))
calcDist(getCorner(WhiteWheat, "topleft"), getCorner(UniformHRW, "topright"), offset = c(35, 39))
calcDist(getCorner(HullessElite, "topleft"), getCorner(UniformBarley, "topright"), offset = c(50, 54))

calcDist(getCorner(HulledObs, "bottomleft"), getCorner(HulledAdvance, "bottomright"), offset = c(-30, -34))

calcDist(getCorner(StateBarley, "bottomright"), getCorner(HulledObs, "bottomleft"), offset = c(-45, -49), digits = 0)

calcDist(getCorner(AckermannMalt, "topright"), getCorner(HulledAdvance, "bottomright"), offset = c(80, 84))
calcDist(getCorner(EasternMalt, "bottomright"), getCorner(EasternMalt, "topright"), offset = c(-15, -11))
calcDist(getCorner(EasternMalt, "bottomleft"), getCorner(EasternMalt, "bottomright"), offset = c(-10, -14))

calcDist(shiftPt(startPt, x = -inx, y =0), getCorner(StateWheat, "bottomleft"), offset = c(-37, -41))

calcDist(shiftPt(startPt, x = 0, y = -iny), getCorner(StateWheat, "bottomleft"), offset = c(54, 64))


calcDist(getCorner(breederSeed, "bottomleft"), getCorner(breederSeed, "bottomright"), offset = c(-10, -14))
calcDist(getCorner(breederSeed, "topright"), getCorner(breederSeed, "bottomright"), offset = c(50, 54))
calcDist(shiftPt(getCorner(breederSeed, "topright"), y = -15), getCorner(breederSeed, "bottomright"), offset = c(65, 69))


labNames <- list(StateWheat = "State Wheat",
					SRWPrelim = "SRW Prelim",
					UniformSouthern = "Uniform Southern",
					Jessica = "Jessica",
					MasonDixon = "Mason Dixon",
					KWSS01 = "KWS-S01",
					UniformEastern = "Uniform Eastern",
					SRWObs = "SRW Obs",
					HRWObs = "HRW Obs",
					WhiteWheat = "White Wheat",
					HRWElite = "HRW Elite",
					HRWPrelim = "HRW Prelim",
					UniformHRW = "Uniform HRW",
					HullessPrelim = "Hulless Prelim",
					HullessObs = "Hulless Obs",
					HullessElite = "Hulless Elite",
					StateBarley = "State Barley",
					UniformBarley = "Uniform Barley",
					HulledObs = "Hulled Obs",
					HulledAdvance = "Hulled Advance",
					HulledPrelim = "Hulled Prelim",
					WinterMalt = "Winter Malt",
					AckermannMalt = "Ackermann Malt",
					EasternMalt = "Eastern Malt",
					breederSeed = "Breeder Seed"
					)

labColors <- cols[trialColors[names(trialPlots)]]

# print(length(colOrder))

shiftLab <- list(StateWheat = 1,
					SRWPrelim = -1,
					UniformSouthern = -1,
					Jessica = 0,
					MasonDixon = -2,
					KWSS01 = 0,
					UniformEastern = -1,
					SRWObs = 0,
					HRWObs = 3,
					WhiteWheat = -3,
					HRWElite = 0,
					HRWPrelim = 5,
					UniformHRW = -1.5,
					HullessPrelim = -3,
					HullessObs = -3,
					HullessElite = 8,
					StateBarley = 2,
					UniformBarley = -5,
					HulledObs = 8,
					HulledAdvance = -8,
					HulledPrelim = -5,
					WinterMalt = -5,
					AckermannMalt = 0,
					EasternMalt = 0,
					breederSeed = 8
)

drawLegend(trialPlots, boundry = rrtrack, labNames = labNames, shiftLeg = c(550, -200), cols = labColors, scaleLeg = 0.4, cex = 0.7)
makeTrialLabels(centers, shiftLab, labNames, cols = labColors, labw = 78, cex = 0.8)
compass(x = 1400, y = 550, rotateWholeMap, xsize = 10, ysize = 20, pointyness = 3, txtSize = 3, col = "black")



text(250, 600, "Kentland Farm", cex = 7)
text(250, 550, "Railroad Track Field", cex = 6)
text(250, 500, "2021-2022", cex = 6)
text(250, 450, "Small Grains Breeding Program", cex = 5)
text(1390, -320, "Drafted by: N. Santantonio, October 8th, 2020" , cex = 2)
text(1390, -290, "Virginia Tech" , cex = 5)



legend(0, -10,  labNames[1:12], fill = labColors[1:12], bty = "n", cex = 2)
legend(250, -10,  labNames[13:length(labNames)], fill = labColors[13:length(labNames)], bty = "n", cex = 2)


dev.off()


