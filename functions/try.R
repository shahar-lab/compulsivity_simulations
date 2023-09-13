for (ind in unique(df1$subject)) {
  df1[df1$subject == ind, "medication"] = as.double(df5[df5$shaharID == ind, "med"])
  }