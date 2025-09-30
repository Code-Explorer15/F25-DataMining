

# HOME WORK 1


#  PACKAGES & SETUP

library(ggplot2)
library(dplyr)
library(tidyr)   #I HAVE INSTALLED ALREADY !!

# Save plots directly in the current HW1 folder
fig_dir <- "."          # save next to this script
data_dir <- "."         # data files are in the same folder as this script

# Helper paths & printing

p   <- function(filename) file.path(data_dir, filename)
say <- function(...) cat(paste0(..., "\n"))
bar <- function(label="") {
  cat("\n", paste(rep("=", 60), collapse=""), "\n", label, "\n",
      paste(rep("=", 60), collapse=""), "\n", sep="")
}

# =========================
#FOR Q1- Data:Su_raw_matrix.txt
# =========================
bar("Q1) Su_raw_matrix.txt")

# Q1(a)

su <- read.delim(p("Su_raw_matrix.txt"), check.names = TRUE, stringsAsFactors = FALSE)
say("Q1(a) Done. Rows: ", nrow(su), " | Columns: ", ncol(su))
say("Q1(a) First column names:"); print(head(names(su), 15))

# Q1(b)

col_name <- "Liver_2.CEL"
if (!col_name %in% names(su)) {
  stop(paste0("Q1(b) Column '", col_name, "' not found. Available columns include: ",
              paste(head(names(su), 30), collapse = ", "), " ..."))
}
liver2_mean <- mean(su[[col_name]], na.rm = TRUE)
liver2_sd   <- sd(su[[col_name]],   na.rm = TRUE)
say("Q1(b) Liver_2.CEL mean = ", liver2_mean)
say("Q1(b) Liver_2.CEL sd   = ", liver2_sd)

# Q1(c)

num_cols <- sapply(su, is.numeric)
su_col_means <- colMeans(su[, num_cols, drop = FALSE], na.rm = TRUE)
su_col_sums  <- colSums(su[, num_cols, drop = FALSE],  na.rm = TRUE)
say("Q1(c) First 10 numeric column means:"); print(head(su_col_means, 10))
say("Q1(c) First 10 numeric column sums :");  print(head(su_col_sums,  10))





# Q2

# where n=10000

set.seed(42)
n <- 10000

# Q2(a) N(0, 0.2)

x_02 <- rnorm(n, mean = 0, sd = 0.2)
png(file.path(fig_dir, "Q2(a)_hist_sigma_0.2.png"), width = 1200, height = 900, res = 150)
hist(x_02, breaks = 50, main = "Q2(a): mean=0, sd=0.2", xlab = "Value", xlim = c(-5, 5))
dev.off()
say("Saved file: Q2(a)_hist_sigma_0.2.png")

# Q2(b) N(0, 0.5)

x_05 <- rnorm(n, mean = 0, sd = 0.5)
png(file.path(fig_dir, "Q2(b)_hist_sigma_0.5.png"), width = 1200, height = 900, res = 150)
hist(x_05, breaks = 50, main = "Q2(b): mean=0, sd=0.5", xlab = "Value", xlim = c(-5, 5))
dev.off()
say("Saved file: Q2(b)_hist_sigma_0.5.png")





# Q3


set.seed(123)
dat <- data.frame(
  cond   = factor(rep(c("A", "B"), each = 200)),
  rating = c(rnorm(200), rnorm(200, mean = .8))
)

## Q3(b) 

plot_q3b <- ggplot(dat, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  ggtitle("Q3(b): Overlaid histograms (dat)")
ggsave(file.path(fig_dir, "Q3(b)_overlaid_hist_dat.png"), plot_q3b, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(b)_overlaid_hist_dat.png")

## Q3(c) 

plot_q3c <- ggplot(dat, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, position = "dodge") +
  ggtitle("Q3(c): Interleaved (dodged) histograms (dat)")
ggsave(file.path(fig_dir, "Q3(c)_interleaved_hist_dat.png"), plot_q3c, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(c)_interleaved_hist_dat.png")

## Q3(d) 

plot_q3d <- ggplot(dat, aes(x = rating, colour = cond)) +
  geom_density() +
  ggtitle("Q3(d): Density (lines) (dat)")
ggsave(file.path(fig_dir, "Q3(d)_density_lines_dat.png"), plot_q3d, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(d)_density_lines_dat.png")

## Q3(e)

plot_q3e <- ggplot(dat, aes(x = rating, fill = cond)) +
  geom_density(alpha = .3) +
  ggtitle("Q3(e): Density (filled) (dat)")
ggsave(file.path(fig_dir, "Q3(e)_density_fill_dat.png"), plot_q3e, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(e)_density_fill_dat.png")

## Q3(f)

diabetes <- read.csv(p("diabetes_train.csv"), stringsAsFactors = FALSE, check.names = TRUE)
if (!all(c("mass", "class") %in% names(diabetes))) {
  stop("Q3(f) Expected columns 'mass' and 'class' not found in diabetes_train.csv.")
}

# Q3(f-b) Overlaid histograms
plot_q3f_b <- ggplot(diabetes, aes(x = mass, fill = class)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  ggtitle("Q3(f-b): Overlaid histograms of 'mass' by 'class' (diabetes)")
ggsave(file.path(fig_dir, "Q3(f-b)_overlaid_hist_diabetes.png"), plot_q3f_b, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(f-b)_overlaid_hist_diabetes.png")

# Q3(f-c) Interleaved (dodged) histograms
plot_q3f_c <- ggplot(diabetes, aes(x = mass, fill = class)) +
  geom_histogram(binwidth = .5, position = "dodge") +
  ggtitle("Q3(f-c): Interleaved histograms of 'mass' by 'class' (diabetes)")
ggsave(file.path(fig_dir, "Q3(f-c)_interleaved_hist_diabetes.png"), plot_q3f_c, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(f-c)_interleaved_hist_diabetes.png")

# Q3(f-d) Density (lines)
plot_q3f_d <- ggplot(diabetes, aes(x = mass, colour = class)) +
  geom_density() +
  ggtitle("Q3(f-d): Density (lines) of 'mass' by 'class' (diabetes)")
ggsave(file.path(fig_dir, "Q3(f-d)_density_lines_diabetes.png"), plot_q3f_d, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(f-d)_density_lines_diabetes.png")

# Q3(f-e) Density (filled)
plot_q3f_e <- ggplot(diabetes, aes(x = mass, fill = class)) +
  geom_density(alpha = .3) +
  ggtitle("Q3(f-e): Density (filled) of 'mass' by 'class' (diabetes)")
ggsave(file.path(fig_dir, "Q3(f-e)_density_fill_diabetes.png"), plot_q3f_e, width = 8, height = 6, dpi = 150)
say("Saved file: Q3(f-e)_density_fill_diabetes.png")

say("Q4")

# Q4) USING titanic.csv 


passengers <- read.csv(p("titanic.csv"), stringsAsFactors = FALSE, check.names = TRUE)

say("Q4(a)")

a_out <- passengers %>% drop_na() %>% summary()
print(a_out)

## Q4(b)

b_out <- passengers %>% filter(Sex == "male")
say("Q4(b) Count of male passengers: ", nrow(b_out))

## Q4(c)
say("Q4(c)")
c_out <- passengers %>% arrange(desc(Fare))
print(head(c_out, 5))

## Q4(d)
say("Q4(d)")
d_out <- passengers %>% mutate(FamSize = Parch + SibSp)
print(head(d_out[, c("Parch", "SibSp", "FamSize")], 5))

## Q4(e)
say("Q4(e)")
e_out <- passengers %>%
  group_by(Sex) %>%
  summarise(meanFare = mean(Fare, na.rm = TRUE),
            numSurv  = sum(Survived, na.rm = TRUE))
print(e_out)



# Q5)USING diabetes.csv
# (a) Report 10th, 30th, 50th, 60th percentiles
# =========================


if (!"skin" %in% names(diabetes)) stop("Q5) Column 'skin' not found in diabetes_train.csv")


## Q5(a)
say("Q5(a) Computing quantiles (10%, 30%, 50%, 60%) for 'skin' with na.rm=TRUE ...")
q_vals <- quantile(diabetes$skin, probs = c(0.10, 0.30, 0.50, 0.60), na.rm = TRUE, names = TRUE)
print(q_vals)

