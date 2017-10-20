setwd('~/Documents/Data Science/honeybees')
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

df <- read_csv('data_jul_oct.csv')
names(df)
str(df$Timestamp)
str(df$Value)
str(df$name)
unique(df$Variable)
activity <- df %>%
  filter(Variable == 'Hive Activity') %>%
  group_by(Timestamp, name) %>%
  summarize(activity = mean(Value, rm.na=T), lat = mean(lat, rm.na = T), lon = mean(lon, rm.na = T))

fanning <- df %>%
  filter(Variable == 'Mean Fanning') %>%
  group_by(Timestamp, name) %>%
  summarize(mean_fanning = mean(Value, rm.na=T))

humidity <- df %>%
  filter(Variable == 'Humidity') %>%
  group_by(Timestamp, name) %>%
  summarize(humidity = mean(Value, rm.na=T))

temp <- df %>%
  filter(Variable == 'Temperature') %>%
  group_by(Timestamp, name) %>%
  summarize(temperature = mean(Value, rm.na=T))

brood <- df %>%
  filter(Variable == 'Brood') %>%
  group_by(Timestamp, name) %>%
  summarize(brood = mean(Value, rm.na=T))

noise <- df %>%
  filter(Variable == 'Mean Flight Noise') %>%
  group_by(Timestamp, name) %>%
  summarize(flight_noise = mean(Value, rm.na=T))

weight <- df %>%
  filter(Variable == 'Weight') %>%
  group_by(Timestamp, name) %>%
  summarize(weight = mean(Value, rm.na = T))

tmp <- activity %>%
  left_join(weight)
tmp <- tmp %>%
  left_join(brood, by = c("Timestamp", "name"))
tmp <- tmp %>%
  left_join(fanning, by = c("Timestamp", "name"))
tmp <- tmp %>%
  left_join(humidity, by = c("Timestamp", "name"))
tmp <- tmp %>%
  left_join(noise, by = c("Timestamp", "name"))
tmp <- tmp %>%
  left_join(temp, by = c("Timestamp", "name"))

df <- tmp

write.csv(df, file = 'bees_cleaned.csv', row.names = F)

rm(tmp, fanning, humidity, noise, sub ,temp, weight, brood, activity)


df %>%
  ggplot(aes(x = Timestamp, y = activity, color = as.factor(name))) +
  geom_point()

sub <- df[df$name == 'beehive_54630',]
library(dplyr)
sub %>%
  filter(Timestamp > as.Date('2017-08-21')) %>%
  ggplot(aes(x = Timestamp, y = activity)) + 
  geom_line()

sub %>%
  ggplot(aes(x = temperature, y = activity)) + 
  geom_point()

sub$week <- week(sub$Timestamp)
sub$day <- yday(sub$Timestamp)
sub %>%
  ggplot(aes(x = day, y = activity)) + 
  geom_col()

df %>%
  ggplot(aes(x = Timestamp, y = activity)) + 
  geom_line() + 
  facet_wrap(~name)

df %>%
  ggplot(aes(x = Timestamp, y = weight)) + 
  geom_line() + 
  facet_wrap(~name)

sub <- df[df$name == 'beehive_9841',]

sub %>%
  ggplot(aes(x = Timestamp, y = activity)) + 
  geom_line()

sub %>%
  ggplot(aes(x = temperature, y = activity)) + 
  geom_point()

sub$week <- week(sub$Timestamp)
sub$day <- yday(sub$Timestamp)
sub %>%
  ggplot(aes(x = day, y = activity)) + 
  geom_col()


