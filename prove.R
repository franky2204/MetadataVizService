# Load a CSV file into R
# Replace 'your_file.csv' with the path to your CSV file

data <- read.csv("input/filtered_data.csv", stringsAsFactors = FALSE)

# View the first few rows

dataErr <- data%>%
    select(c(id,category,bmi, sequencing_batch, lesion_burden))
write.csv(dataErr, "input/dataErr1.csv", row.names = FALSE)
dataErr$lesion_burden <- ifelse(dataErr$lesion_burden == 1, TRUE, FALSE)
write.csv(dataErr, "input/dataErr2.csv", row.names = FALSE)

print(unique(data$treg_cd39_pos))
