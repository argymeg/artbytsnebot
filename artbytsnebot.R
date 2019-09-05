#!/usr/bin/Rscript
library(jsonlite)
library(R.utils)
library(imager)
library(data.table)
library(Rtsne)
library(ggplot2)
library(rtweet)

timestamp <- as.numeric(Sys.time())

pixabay_api_key = scan("pixabay_api_key.txt", what = character())
twitter_api_keys <- scan("twitter_api_keys.txt", what = character())

queryurl = paste0("https://pixabay.com/api/?key=", pixabay_api_key, "&page=", sample(1:25, 1), "&safesearch=true")
selectionindex <- sample(1:20, 1)
queryresults <- fromJSON(queryurl)
imageurl <- queryresults$hits$largeImageURL[selectionindex]
pageurl <- queryresults$hits$pageURL[selectionindex]
caption <- capitalize(queryresults$hits$tags[selectionindex])

original <- load.image(imageurl)
save.image(original, paste0(timestamp, "-original.png"))

imagedt <- as.data.table(as.data.frame(original))
imagedt <- dcast(imagedt, x + y ~ cc)
imagedt <- unique(imagedt, by = c("1","2","3"))

message("Running t-SNE, this will take a sec")
imagetsne <- Rtsne(imagedt[,3:5], num_threads = 0)

colourgradient <- abs(imagedt$x - round(median(range(imagedt$x)))) + abs(imagedt$y - round(median(range(imagedt$y))))

ncolours <- 5
randomcolours <- replicate(ncolours, paste0("#", paste0(sample(c(0:9, LETTERS[1:6]), 6, T), collapse = '')))

plot_df <- data.frame(x = imagetsne$Y[,1], y = imagetsne$Y[,2], col = colourgradient)
plot_out <- ggplot(plot_df) + geom_point(aes(x=x, y=y, color=col)) + scale_color_gradientn(colours = randomcolours) +
  theme(legend.position = "none", axis.title = element_blank(), panel.background = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(paste0(timestamp, "-plot.png"), plot_out)

plot_back_in <- load.image(paste0(timestamp, "-plot.png"))

# Blurring tends to error out on the first attempt - not sure why
for(i in 1:5){
  message("Blurring: attempt #", i)
  try({
    plot_blurred <- blur_anisotropic(plot_back_in, 1000)
    message("Success")
    break
  })
}
save.image(plot_blurred, paste0(timestamp, "-plot_blurred.png"))
# base::save.image(paste0(timestamp, "-workspace.RData"))

token <- create_token(
  app = "artbytsnebot",
  consumer_key = twitter_api_keys[1],
  consumer_secret = twitter_api_keys[2],
  access_token = twitter_api_keys[3],
  access_secret = twitter_api_keys[4])
post_tweet(paste(caption, pageurl, sep = "\n"), media = paste0(timestamp, "-plot_blurred.png"), token)
