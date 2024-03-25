
# Set working directory
# setwd()


new_data <- read.table('tidytutorial.dat', sep=' ')
names(new_data) <- c('R0000100',
'R0536300',
'R0536401',
'R0536402',
'R1235800',
'R1302400',
'R1302500',
'R1302600',
'R1302700',
'R1482600',
'U0008700',
'U0008900',
'U0014500',
'U0015000',
'U1845300',
'U1845500',
'U1852300',
'U1853200',
'U3443800',
'U3444000',
'U3451400',
'U3453600',
'U4949500',
'U4949700',
'U4954500',
'U4956900',
'Z9083900',
'Z9084400',
'Z9084500',
'Z9084600',
'Z9084700',
'Z9085100',
'Z9121900',
'Z9141400',
'Z9164500')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0536300 <- factor(data$R0536300,
levels=c(0.0,1.0,2.0),
labels=c("No Information",
"Male",
"Female"))
  data$R0536401 <- factor(data$R0536401,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
labels=c("1: January",
"2: February",
"3: March",
"4: April",
"5: May",
"6: June",
"7: July",
"8: August",
"9: September",
"10: October",
"11: November",
"12: December"))
  data$R1235800 <- factor(data$R1235800,
levels=c(0.0,1.0),
labels=c("Oversample",
"Cross-sectional"))
  data$R1302400 <- factor(data$R1302400,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302500 <- factor(data$R1302500,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302600 <- factor(data$R1302600,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302700 <- factor(data$R1302700,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1482600 <- factor(data$R1482600,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Black",
"Hispanic",
"Mixed Race (Non-Hispanic)",
"Non-Black / Non-Hispanic"))
  data$U0008700 <- factor(data$U0008700,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$U0014500 <- factor(data$U0014500,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U0015000 <- factor(data$U0015000,
levels=c(0.0,1.0,2.0),
labels=c("Rural",
"Urban",
"Unknown"))
  data$U1845300 <- factor(data$U1845300,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$U1852300 <- factor(data$U1852300,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U1853200 <- factor(data$U1853200,
levels=c(0.0,1.0,2.0),
labels=c("Rural",
"Urban",
"Unknown"))
  data$U3443800 <- factor(data$U3443800,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$U3451400 <- factor(data$U3451400,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U3453600 <- factor(data$U3453600,
levels=c(0.0,1.0,2.0),
labels=c("Rural",
"Urban",
"Unknown"))
  data$U4949500 <- factor(data$U4949500,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$U4954500 <- factor(data$U4954500,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0),
labels=c("Never married, cohabiting",
"Never married, not cohabiting",
"Married, spouse present",
"Married, spouse absent",
"Separated, cohabiting",
"Separated, not cohabiting",
"Divorced, cohabiting",
"Divorced, not cohabiting",
"Widowed, cohabiting",
"Widowed, not cohabiting"))
  data$U4956900 <- factor(data$U4956900,
levels=c(0.0,1.0,2.0),
labels=c("Rural",
"Urban",
"Unknown"))
  data$Z9083900 <- factor(data$Z9083900,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0),
labels=c("None",
"GED",
"High school diploma (Regular 12 year program)",
"Associate/Junior college (AA)",
"Bachelor's degree (BA, BS)",
"Master's degree (MA, MS)",
"PhD",
"Professional degree (DDS, JD, MD)"))
  data$Z9085100 <- factor(data$Z9085100,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0),
labels=c("Round 1",
"Round 2",
"Round 3",
"Round 4",
"Round 5",
"Round 6",
"Round 7",
"Round 8",
"Round 9",
"Round 10",
"Round 11",
"Round 12",
"Round 13",
"Round 14",
"Round 15",
"Round 16",
"Round 17",
"Round 18",
"Round 19",
"Round 20"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
data$R0000100 <- factor(data$R0000100,
levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0),
labels=c("0",
"1 TO 999",
"1000 TO 1999",
"2000 TO 2999",
"3000 TO 3999",
"4000 TO 4999",
"5000 TO 5999",
"6000 TO 6999",
"7000 TO 7999",
"8000 TO 8999",
"9000 TO 9999"))
data$U0008900[-999999.0 <= data$U0008900 & data$U0008900 <= -3000.0] <- -999999.0
data$U0008900[-2999.0 <= data$U0008900 & data$U0008900 <= -2000.0] <- -2999.0
data$U0008900[-1999.0 <= data$U0008900 & data$U0008900 <= -1000.0] <- -1999.0
data$U0008900[-999.0 <= data$U0008900 & data$U0008900 <= -1.0] <- -999.0
data$U0008900[1.0 <= data$U0008900 & data$U0008900 <= 1000.0] <- 1.0
data$U0008900[1001.0 <= data$U0008900 & data$U0008900 <= 2000.0] <- 1001.0
data$U0008900[2001.0 <= data$U0008900 & data$U0008900 <= 3000.0] <- 2001.0
data$U0008900[3001.0 <= data$U0008900 & data$U0008900 <= 5000.0] <- 3001.0
data$U0008900[5001.0 <= data$U0008900 & data$U0008900 <= 10000.0] <- 5001.0
data$U0008900[10001.0 <= data$U0008900 & data$U0008900 <= 20000.0] <- 10001.0
data$U0008900[20001.0 <= data$U0008900 & data$U0008900 <= 30000.0] <- 20001.0
data$U0008900[30001.0 <= data$U0008900 & data$U0008900 <= 40000.0] <- 30001.0
data$U0008900[40001.0 <= data$U0008900 & data$U0008900 <= 50000.0] <- 40001.0
data$U0008900[50001.0 <= data$U0008900 & data$U0008900 <= 65000.0] <- 50001.0
data$U0008900[65001.0 <= data$U0008900 & data$U0008900 <= 80000.0] <- 65001.0
data$U0008900[80001.0 <= data$U0008900 & data$U0008900 <= 100000.0] <- 80001.0
data$U0008900[100001.0 <= data$U0008900 & data$U0008900 <= 150000.0] <- 100001.0
data$U0008900[150001.0 <= data$U0008900 & data$U0008900 <= 200000.0] <- 150001.0
data$U0008900[200001.0 <= data$U0008900 & data$U0008900 <= 999999.0] <- 200001.0
data$U0008900 <- factor(data$U0008900,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$U1845500[-999999.0 <= data$U1845500 & data$U1845500 <= -3000.0] <- -999999.0
data$U1845500[-2999.0 <= data$U1845500 & data$U1845500 <= -2000.0] <- -2999.0
data$U1845500[-1999.0 <= data$U1845500 & data$U1845500 <= -1000.0] <- -1999.0
data$U1845500[-999.0 <= data$U1845500 & data$U1845500 <= -1.0] <- -999.0
data$U1845500[1.0 <= data$U1845500 & data$U1845500 <= 1000.0] <- 1.0
data$U1845500[1001.0 <= data$U1845500 & data$U1845500 <= 2000.0] <- 1001.0
data$U1845500[2001.0 <= data$U1845500 & data$U1845500 <= 3000.0] <- 2001.0
data$U1845500[3001.0 <= data$U1845500 & data$U1845500 <= 5000.0] <- 3001.0
data$U1845500[5001.0 <= data$U1845500 & data$U1845500 <= 10000.0] <- 5001.0
data$U1845500[10001.0 <= data$U1845500 & data$U1845500 <= 20000.0] <- 10001.0
data$U1845500[20001.0 <= data$U1845500 & data$U1845500 <= 30000.0] <- 20001.0
data$U1845500[30001.0 <= data$U1845500 & data$U1845500 <= 40000.0] <- 30001.0
data$U1845500[40001.0 <= data$U1845500 & data$U1845500 <= 50000.0] <- 40001.0
data$U1845500[50001.0 <= data$U1845500 & data$U1845500 <= 65000.0] <- 50001.0
data$U1845500[65001.0 <= data$U1845500 & data$U1845500 <= 80000.0] <- 65001.0
data$U1845500[80001.0 <= data$U1845500 & data$U1845500 <= 100000.0] <- 80001.0
data$U1845500[100001.0 <= data$U1845500 & data$U1845500 <= 150000.0] <- 100001.0
data$U1845500[150001.0 <= data$U1845500 & data$U1845500 <= 200000.0] <- 150001.0
data$U1845500[200001.0 <= data$U1845500 & data$U1845500 <= 999999.0] <- 200001.0
data$U1845500 <- factor(data$U1845500,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$U3444000[-999999.0 <= data$U3444000 & data$U3444000 <= -3000.0] <- -999999.0
data$U3444000[-2999.0 <= data$U3444000 & data$U3444000 <= -2000.0] <- -2999.0
data$U3444000[-1999.0 <= data$U3444000 & data$U3444000 <= -1000.0] <- -1999.0
data$U3444000[-999.0 <= data$U3444000 & data$U3444000 <= -1.0] <- -999.0
data$U3444000[1.0 <= data$U3444000 & data$U3444000 <= 1000.0] <- 1.0
data$U3444000[1001.0 <= data$U3444000 & data$U3444000 <= 2000.0] <- 1001.0
data$U3444000[2001.0 <= data$U3444000 & data$U3444000 <= 3000.0] <- 2001.0
data$U3444000[3001.0 <= data$U3444000 & data$U3444000 <= 5000.0] <- 3001.0
data$U3444000[5001.0 <= data$U3444000 & data$U3444000 <= 10000.0] <- 5001.0
data$U3444000[10001.0 <= data$U3444000 & data$U3444000 <= 20000.0] <- 10001.0
data$U3444000[20001.0 <= data$U3444000 & data$U3444000 <= 30000.0] <- 20001.0
data$U3444000[30001.0 <= data$U3444000 & data$U3444000 <= 40000.0] <- 30001.0
data$U3444000[40001.0 <= data$U3444000 & data$U3444000 <= 50000.0] <- 40001.0
data$U3444000[50001.0 <= data$U3444000 & data$U3444000 <= 65000.0] <- 50001.0
data$U3444000[65001.0 <= data$U3444000 & data$U3444000 <= 80000.0] <- 65001.0
data$U3444000[80001.0 <= data$U3444000 & data$U3444000 <= 100000.0] <- 80001.0
data$U3444000[100001.0 <= data$U3444000 & data$U3444000 <= 150000.0] <- 100001.0
data$U3444000[150001.0 <= data$U3444000 & data$U3444000 <= 200000.0] <- 150001.0
data$U3444000[200001.0 <= data$U3444000 & data$U3444000 <= 999999.0] <- 200001.0
data$U3444000 <- factor(data$U3444000,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$U4949700[-999999.0 <= data$U4949700 & data$U4949700 <= -3000.0] <- -999999.0
data$U4949700[-2999.0 <= data$U4949700 & data$U4949700 <= -2000.0] <- -2999.0
data$U4949700[-1999.0 <= data$U4949700 & data$U4949700 <= -1000.0] <- -1999.0
data$U4949700[-999.0 <= data$U4949700 & data$U4949700 <= -1.0] <- -999.0
data$U4949700[1.0 <= data$U4949700 & data$U4949700 <= 1000.0] <- 1.0
data$U4949700[1001.0 <= data$U4949700 & data$U4949700 <= 2000.0] <- 1001.0
data$U4949700[2001.0 <= data$U4949700 & data$U4949700 <= 3000.0] <- 2001.0
data$U4949700[3001.0 <= data$U4949700 & data$U4949700 <= 5000.0] <- 3001.0
data$U4949700[5001.0 <= data$U4949700 & data$U4949700 <= 10000.0] <- 5001.0
data$U4949700[10001.0 <= data$U4949700 & data$U4949700 <= 20000.0] <- 10001.0
data$U4949700[20001.0 <= data$U4949700 & data$U4949700 <= 30000.0] <- 20001.0
data$U4949700[30001.0 <= data$U4949700 & data$U4949700 <= 40000.0] <- 30001.0
data$U4949700[40001.0 <= data$U4949700 & data$U4949700 <= 50000.0] <- 40001.0
data$U4949700[50001.0 <= data$U4949700 & data$U4949700 <= 65000.0] <- 50001.0
data$U4949700[65001.0 <= data$U4949700 & data$U4949700 <= 80000.0] <- 65001.0
data$U4949700[80001.0 <= data$U4949700 & data$U4949700 <= 100000.0] <- 80001.0
data$U4949700[100001.0 <= data$U4949700 & data$U4949700 <= 150000.0] <- 100001.0
data$U4949700[150001.0 <= data$U4949700 & data$U4949700 <= 200000.0] <- 150001.0
data$U4949700[200001.0 <= data$U4949700 & data$U4949700 <= 999999.0] <- 200001.0
data$U4949700 <- factor(data$U4949700,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$Z9084400[0.0 <= data$Z9084400 & data$Z9084400 <= 200.0] <- 0.0
data$Z9084400[201.0 <= data$Z9084400 & data$Z9084400 <= 220.0] <- 201.0
data$Z9084400[221.0 <= data$Z9084400 & data$Z9084400 <= 240.0] <- 221.0
data$Z9084400[241.0 <= data$Z9084400 & data$Z9084400 <= 260.0] <- 241.0
data$Z9084400[261.0 <= data$Z9084400 & data$Z9084400 <= 280.0] <- 261.0
data$Z9084400[281.0 <= data$Z9084400 & data$Z9084400 <= 300.0] <- 281.0
data$Z9084400[301.0 <= data$Z9084400 & data$Z9084400 <= 320.0] <- 301.0
data$Z9084400[321.0 <= data$Z9084400 & data$Z9084400 <= 340.0] <- 321.0
data$Z9084400[341.0 <= data$Z9084400 & data$Z9084400 <= 360.0] <- 341.0
data$Z9084400[361.0 <= data$Z9084400 & data$Z9084400 <= 380.0] <- 361.0
data$Z9084400[381.0 <= data$Z9084400 & data$Z9084400 <= 400.0] <- 381.0
data$Z9084400[401.0 <= data$Z9084400 & data$Z9084400 <= 420.0] <- 401.0
data$Z9084400[421.0 <= data$Z9084400 & data$Z9084400 <= 440.0] <- 421.0
data$Z9084400[441.0 <= data$Z9084400 & data$Z9084400 <= 460.0] <- 441.0
data$Z9084400[461.0 <= data$Z9084400 & data$Z9084400 <= 480.0] <- 461.0
data$Z9084400[481.0 <= data$Z9084400 & data$Z9084400 <= 500.0] <- 481.0
data$Z9084400[501.0 <= data$Z9084400 & data$Z9084400 <= 520.0] <- 501.0
data$Z9084400 <- factor(data$Z9084400,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084500[0.0 <= data$Z9084500 & data$Z9084500 <= 200.0] <- 0.0
data$Z9084500[201.0 <= data$Z9084500 & data$Z9084500 <= 220.0] <- 201.0
data$Z9084500[221.0 <= data$Z9084500 & data$Z9084500 <= 240.0] <- 221.0
data$Z9084500[241.0 <= data$Z9084500 & data$Z9084500 <= 260.0] <- 241.0
data$Z9084500[261.0 <= data$Z9084500 & data$Z9084500 <= 280.0] <- 261.0
data$Z9084500[281.0 <= data$Z9084500 & data$Z9084500 <= 300.0] <- 281.0
data$Z9084500[301.0 <= data$Z9084500 & data$Z9084500 <= 320.0] <- 301.0
data$Z9084500[321.0 <= data$Z9084500 & data$Z9084500 <= 340.0] <- 321.0
data$Z9084500[341.0 <= data$Z9084500 & data$Z9084500 <= 360.0] <- 341.0
data$Z9084500[361.0 <= data$Z9084500 & data$Z9084500 <= 380.0] <- 361.0
data$Z9084500[381.0 <= data$Z9084500 & data$Z9084500 <= 400.0] <- 381.0
data$Z9084500[401.0 <= data$Z9084500 & data$Z9084500 <= 420.0] <- 401.0
data$Z9084500[421.0 <= data$Z9084500 & data$Z9084500 <= 440.0] <- 421.0
data$Z9084500[441.0 <= data$Z9084500 & data$Z9084500 <= 460.0] <- 441.0
data$Z9084500[461.0 <= data$Z9084500 & data$Z9084500 <= 480.0] <- 461.0
data$Z9084500[481.0 <= data$Z9084500 & data$Z9084500 <= 500.0] <- 481.0
data$Z9084500[501.0 <= data$Z9084500 & data$Z9084500 <= 520.0] <- 501.0
data$Z9084500 <- factor(data$Z9084500,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084600[0.0 <= data$Z9084600 & data$Z9084600 <= 200.0] <- 0.0
data$Z9084600[201.0 <= data$Z9084600 & data$Z9084600 <= 220.0] <- 201.0
data$Z9084600[221.0 <= data$Z9084600 & data$Z9084600 <= 240.0] <- 221.0
data$Z9084600[241.0 <= data$Z9084600 & data$Z9084600 <= 260.0] <- 241.0
data$Z9084600[261.0 <= data$Z9084600 & data$Z9084600 <= 280.0] <- 261.0
data$Z9084600[281.0 <= data$Z9084600 & data$Z9084600 <= 300.0] <- 281.0
data$Z9084600[301.0 <= data$Z9084600 & data$Z9084600 <= 320.0] <- 301.0
data$Z9084600[321.0 <= data$Z9084600 & data$Z9084600 <= 340.0] <- 321.0
data$Z9084600[341.0 <= data$Z9084600 & data$Z9084600 <= 360.0] <- 341.0
data$Z9084600[361.0 <= data$Z9084600 & data$Z9084600 <= 380.0] <- 361.0
data$Z9084600[381.0 <= data$Z9084600 & data$Z9084600 <= 400.0] <- 381.0
data$Z9084600[401.0 <= data$Z9084600 & data$Z9084600 <= 420.0] <- 401.0
data$Z9084600[421.0 <= data$Z9084600 & data$Z9084600 <= 440.0] <- 421.0
data$Z9084600[441.0 <= data$Z9084600 & data$Z9084600 <= 460.0] <- 441.0
data$Z9084600[461.0 <= data$Z9084600 & data$Z9084600 <= 480.0] <- 461.0
data$Z9084600[481.0 <= data$Z9084600 & data$Z9084600 <= 500.0] <- 481.0
data$Z9084600[501.0 <= data$Z9084600 & data$Z9084600 <= 520.0] <- 501.0
data$Z9084600 <- factor(data$Z9084600,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084700[0.0 <= data$Z9084700 & data$Z9084700 <= 200.0] <- 0.0
data$Z9084700[201.0 <= data$Z9084700 & data$Z9084700 <= 220.0] <- 201.0
data$Z9084700[221.0 <= data$Z9084700 & data$Z9084700 <= 240.0] <- 221.0
data$Z9084700[241.0 <= data$Z9084700 & data$Z9084700 <= 260.0] <- 241.0
data$Z9084700[261.0 <= data$Z9084700 & data$Z9084700 <= 280.0] <- 261.0
data$Z9084700[281.0 <= data$Z9084700 & data$Z9084700 <= 300.0] <- 281.0
data$Z9084700[301.0 <= data$Z9084700 & data$Z9084700 <= 320.0] <- 301.0
data$Z9084700[321.0 <= data$Z9084700 & data$Z9084700 <= 340.0] <- 321.0
data$Z9084700[341.0 <= data$Z9084700 & data$Z9084700 <= 360.0] <- 341.0
data$Z9084700[361.0 <= data$Z9084700 & data$Z9084700 <= 380.0] <- 361.0
data$Z9084700[381.0 <= data$Z9084700 & data$Z9084700 <= 400.0] <- 381.0
data$Z9084700[401.0 <= data$Z9084700 & data$Z9084700 <= 420.0] <- 401.0
data$Z9084700[421.0 <= data$Z9084700 & data$Z9084700 <= 440.0] <- 421.0
data$Z9084700[441.0 <= data$Z9084700 & data$Z9084700 <= 460.0] <- 441.0
data$Z9084700[461.0 <= data$Z9084700 & data$Z9084700 <= 480.0] <- 461.0
data$Z9084700[481.0 <= data$Z9084700 & data$Z9084700 <= 500.0] <- 481.0
data$Z9084700[501.0 <= data$Z9084700 & data$Z9084700 <= 520.0] <- 501.0
data$Z9084700 <- factor(data$Z9084700,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9121900[-9999999.0 <= data$Z9121900 & data$Z9121900 <= -3000.0] <- -9999999.0
data$Z9121900[-2999.0 <= data$Z9121900 & data$Z9121900 <= -2000.0] <- -2999.0
data$Z9121900[-1999.0 <= data$Z9121900 & data$Z9121900 <= -1000.0] <- -1999.0
data$Z9121900[-999.0 <= data$Z9121900 & data$Z9121900 <= -1.0] <- -999.0
data$Z9121900[1.0 <= data$Z9121900 & data$Z9121900 <= 1000.0] <- 1.0
data$Z9121900[1001.0 <= data$Z9121900 & data$Z9121900 <= 2000.0] <- 1001.0
data$Z9121900[2001.0 <= data$Z9121900 & data$Z9121900 <= 3000.0] <- 2001.0
data$Z9121900[3001.0 <= data$Z9121900 & data$Z9121900 <= 5000.0] <- 3001.0
data$Z9121900[5001.0 <= data$Z9121900 & data$Z9121900 <= 10000.0] <- 5001.0
data$Z9121900[10001.0 <= data$Z9121900 & data$Z9121900 <= 20000.0] <- 10001.0
data$Z9121900[20001.0 <= data$Z9121900 & data$Z9121900 <= 30000.0] <- 20001.0
data$Z9121900[30001.0 <= data$Z9121900 & data$Z9121900 <= 40000.0] <- 30001.0
data$Z9121900[40001.0 <= data$Z9121900 & data$Z9121900 <= 50000.0] <- 40001.0
data$Z9121900[50001.0 <= data$Z9121900 & data$Z9121900 <= 65000.0] <- 50001.0
data$Z9121900[65001.0 <= data$Z9121900 & data$Z9121900 <= 80000.0] <- 65001.0
data$Z9121900[80001.0 <= data$Z9121900 & data$Z9121900 <= 100000.0] <- 80001.0
data$Z9121900[100001.0 <= data$Z9121900 & data$Z9121900 <= 150000.0] <- 100001.0
data$Z9121900[150001.0 <= data$Z9121900 & data$Z9121900 <= 200000.0] <- 150001.0
data$Z9121900[200001.0 <= data$Z9121900 & data$Z9121900 <= 9.99999999E8] <- 200001.0
data$Z9121900 <- factor(data$Z9121900,
levels=c(-9999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-9999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999999: 200001+"))
data$Z9141400[-9999999.0 <= data$Z9141400 & data$Z9141400 <= -3000.0] <- -9999999.0
data$Z9141400[-2999.0 <= data$Z9141400 & data$Z9141400 <= -2000.0] <- -2999.0
data$Z9141400[-1999.0 <= data$Z9141400 & data$Z9141400 <= -1000.0] <- -1999.0
data$Z9141400[-999.0 <= data$Z9141400 & data$Z9141400 <= -1.0] <- -999.0
data$Z9141400[1.0 <= data$Z9141400 & data$Z9141400 <= 1000.0] <- 1.0
data$Z9141400[1001.0 <= data$Z9141400 & data$Z9141400 <= 2000.0] <- 1001.0
data$Z9141400[2001.0 <= data$Z9141400 & data$Z9141400 <= 3000.0] <- 2001.0
data$Z9141400[3001.0 <= data$Z9141400 & data$Z9141400 <= 5000.0] <- 3001.0
data$Z9141400[5001.0 <= data$Z9141400 & data$Z9141400 <= 10000.0] <- 5001.0
data$Z9141400[10001.0 <= data$Z9141400 & data$Z9141400 <= 20000.0] <- 10001.0
data$Z9141400[20001.0 <= data$Z9141400 & data$Z9141400 <= 30000.0] <- 20001.0
data$Z9141400[30001.0 <= data$Z9141400 & data$Z9141400 <= 40000.0] <- 30001.0
data$Z9141400[40001.0 <= data$Z9141400 & data$Z9141400 <= 50000.0] <- 40001.0
data$Z9141400[50001.0 <= data$Z9141400 & data$Z9141400 <= 65000.0] <- 50001.0
data$Z9141400[65001.0 <= data$Z9141400 & data$Z9141400 <= 80000.0] <- 65001.0
data$Z9141400[80001.0 <= data$Z9141400 & data$Z9141400 <= 100000.0] <- 80001.0
data$Z9141400[100001.0 <= data$Z9141400 & data$Z9141400 <= 150000.0] <- 100001.0
data$Z9141400[150001.0 <= data$Z9141400 & data$Z9141400 <= 200000.0] <- 150001.0
data$Z9141400[200001.0 <= data$Z9141400 & data$Z9141400 <= 9.99999999E8] <- 200001.0
data$Z9141400 <- factor(data$Z9141400,
levels=c(-9999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-9999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999999: 200001+"))
data$Z9164500[-9999999.0 <= data$Z9164500 & data$Z9164500 <= -3000.0] <- -9999999.0
data$Z9164500[-2999.0 <= data$Z9164500 & data$Z9164500 <= -2000.0] <- -2999.0
data$Z9164500[-1999.0 <= data$Z9164500 & data$Z9164500 <= -1000.0] <- -1999.0
data$Z9164500[-999.0 <= data$Z9164500 & data$Z9164500 <= -1.0] <- -999.0
data$Z9164500[1.0 <= data$Z9164500 & data$Z9164500 <= 1000.0] <- 1.0
data$Z9164500[1001.0 <= data$Z9164500 & data$Z9164500 <= 2000.0] <- 1001.0
data$Z9164500[2001.0 <= data$Z9164500 & data$Z9164500 <= 3000.0] <- 2001.0
data$Z9164500[3001.0 <= data$Z9164500 & data$Z9164500 <= 5000.0] <- 3001.0
data$Z9164500[5001.0 <= data$Z9164500 & data$Z9164500 <= 10000.0] <- 5001.0
data$Z9164500[10001.0 <= data$Z9164500 & data$Z9164500 <= 20000.0] <- 10001.0
data$Z9164500[20001.0 <= data$Z9164500 & data$Z9164500 <= 30000.0] <- 20001.0
data$Z9164500[30001.0 <= data$Z9164500 & data$Z9164500 <= 40000.0] <- 30001.0
data$Z9164500[40001.0 <= data$Z9164500 & data$Z9164500 <= 50000.0] <- 40001.0
data$Z9164500[50001.0 <= data$Z9164500 & data$Z9164500 <= 65000.0] <- 50001.0
data$Z9164500[65001.0 <= data$Z9164500 & data$Z9164500 <= 80000.0] <- 65001.0
data$Z9164500[80001.0 <= data$Z9164500 & data$Z9164500 <= 100000.0] <- 80001.0
data$Z9164500[100001.0 <= data$Z9164500 & data$Z9164500 <= 150000.0] <- 100001.0
data$Z9164500[150001.0 <= data$Z9164500 & data$Z9164500 <= 200000.0] <- 150001.0
data$Z9164500[200001.0 <= data$Z9164500 & data$Z9164500 <= 9.99999999E8] <- 200001.0
data$Z9164500 <- factor(data$Z9164500,
levels=c(-9999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-9999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999999: 200001+"))
return(data)
}

varlabels <- c("PUBID - YTH ID CODE 1997",
"KEY!SEX (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"CV_SAMPLE_TYPE 1997",
"CV_HGC_BIO_DAD 1997",
"CV_HGC_BIO_MOM 1997",
"CV_HGC_RES_DAD 1997",
"CV_HGC_RES_MOM 1997",
"KEY!RACE_ETHNICITY (SYMBOL) 1997",
"CV_ENROLLSTAT 2015",
"CV_INCOME_FAMILY 2015",
"CV_MARSTAT 2015",
"CV_URBAN-RURAL 2015",
"CV_ENROLLSTAT 2017",
"CV_INCOME_FAMILY 2017",
"CV_MARSTAT 2017",
"CV_URBAN-RURAL 2017",
"CV_ENROLLSTAT 2019",
"CV_INCOME_FAMILY 2019",
"CV_MARSTAT 2019",
"CV_URBAN-RURAL 2019",
"CV_ENROLLSTAT 2021",
"CV_INCOME_FAMILY 2021",
"CV_MARSTAT 2021",
"CV_URBAN-RURAL 2021",
"CVC_HIGHEST_DEGREE_EVER",
"CVC_BA_DEGREE",
"CVC_PROF_DEGREE",
"CVC_PHD_DEGREE",
"CVC_MA_DEGREE",
"CVC_RND",
"CVC_HH_NET_WORTH_30",
"CVC_HH_NET_WORTH_35",
"CVC_HH_NET_WORTH_40"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("PUBID_1997",
"KEY_SEX_1997",
"KEY_BDATE_M_1997",
"KEY_BDATE_Y_1997",
"CV_SAMPLE_TYPE_1997",
"CV_HGC_BIO_DAD_1997",
"CV_HGC_BIO_MOM_1997",
"CV_HGC_RES_DAD_1997",
"CV_HGC_RES_MOM_1997",
"KEY_RACE_ETHNICITY_1997",
"CV_ENROLLSTAT_2015",
"CV_INCOME_FAMILY_2015",
"CV_MARSTAT_2015",
"CV_URBAN-RURAL_2015",
"CV_ENROLLSTAT_2017",
"CV_INCOME_FAMILY_2017",
"CV_MARSTAT_2017",
"CV_URBAN-RURAL_2017",
"CV_ENROLLSTAT_2019",
"CV_INCOME_FAMILY_2019",
"CV_MARSTAT_2019",
"CV_URBAN-RURAL_2019",
"CV_ENROLLSTAT_2021",
"CV_INCOME_FAMILY_2021",
"CV_MARSTAT_2021",
"CV_URBAN-RURAL_2021",
"CVC_HIGHEST_DEGREE_EVER_XRND",
"CVC_BA_DEGREE_XRND",
"CVC_PROF_DEGREE_XRND",
"CVC_PHD_DEGREE_XRND",
"CVC_MA_DEGREE_XRND",
"CVC_RND_XRND",
"CVC_HH_NET_WORTH_30_XRND",
"CVC_HH_NET_WORTH_35_XRND",
"CVC_HH_NET_WORTH_40_XRND")
return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
#categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
#new_data <- qnames(new_data)
#categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#categories <- vallabels_continuous(new_data)
#summary(categories)

#************************************************************************************************************

