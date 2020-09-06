source("Function_call.R")
#source("test.R")
#Latest File in GC
GC <- "All visitors_Sep_4_2020.xlsx"
#Daily All Visitor data in VJ
offpage(GC)
offsite(GC)
onsite(GC)
overall(GC)
#Generating Details 
DETAILS()
#Keyword
keyword()

#Weekly data on fridays
asset_conversions(GC)
web_inquiries(GC)
