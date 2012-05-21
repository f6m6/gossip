g_exists <- file.exists("gossip_propagation.dat")
ae_exists <- file.exists("antientropy_propagation.dat")
s_exists <- file.exists("serial_propagation.dat")
n_exists <-  file.exists("neighbour_propagation.dat")

if (g_exists) g <- read.table("gossip_propagation.dat", sep="\t") else g <- NULL
if (s_exists) s <- read.table("serial_propagation.dat", sep="\t") else s <- NULL
if (ae_exists) ae <- read.table("antientropy_propagation.dat", sep="\t") else ae <- NULL
if (n_exists) n <- read.table("neighbour_propagation.dat", sep="\t") else n <- NULL

if (g_exists) gtraf <- read.table("gossip_traffic.dat", sep="\t") else gtraf <- NULL
if (ae_exists) aetraf <- read.table("antientropy_traffic.dat", sep="\t") else aetraf <- NULL
if (s_exists) straf <- read.table("serial_traffic.dat", sep="\t") else straf <- NULL
if (n_exists) ntraf <- read.table("neighbour_traffic.dat", sep="\t") else ntraf <- NULL

pdf(file="propagation.pdf")
x_max <- max(max(g[1]),max(s[1]),max(ae[1]), max(n[1]))
y_max <- max(max(g[2]),max(s[2]),max(ae[2]),max(n[2]))
plot(s, type="n", ann=FALSE, xlim=range(0, x_max), ylim=range(0, y_max))
if (s_exists) lines(s, type="l")
if (g_exists) lines(g, type="l", col="blue")
if (ae_exists) lines(ae, type="l", col="green")
if (n_exists) lines(n, type="l", col="red")
title(ylab="nodes reached", xlab="time")
dev.off()

pdf(file="traffic.pdf")
x_max <- max(max(gtraf[1]),max(straf[1]),max(aetraf[1]), max(ntraf[1]))
y_max <- max(max(gtraf[2]),max(straf[2]),max(aetraf[2]),max(ntraf[2]))
plot(straf, type="n", ylim=range(0, y_max), xlim=range(0, x_max), ann=FALSE)
if (s_exists) lines(straf, type="l")
if (g_exists) lines(gtraf, type="l", col="blue")
if (ae_exists) lines(aetraf, type="l", col="green")
if (n_exists) lines(ntraf, type="l", col="red")
title(ylab="network events in queue", xlab="time")
dev.off()
