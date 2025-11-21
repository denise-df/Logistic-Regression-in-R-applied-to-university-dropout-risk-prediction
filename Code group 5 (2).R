## IMPORT OF DATASET AND REQUIRED PACKAGES
library(readxl)
data <- read_excel("~/Luiss/Luiss - Year One/Advanced Statistics/dataset2.xlsx",
                   col_types = c("numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "text"))


library(readxl)
library(caret)
library(dplyr)
library(MASS)
library(ggplot2)

## CUSTOM PALETTE
custom_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white", colour = "lightblue"),
    panel.grid.major = element_line(colour = "lightblue1"),
    panel.grid.minor = element_line(colour = "lightblue2"),
    panel.border = element_rect(colour = "blue3", fill = NA, size = 1),
    axis.text = element_text(size = 10, colour = "blue4"),
    axis.title = element_text(size = 12, face = "bold", colour = "blue4"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, colour = "blue4", family = "sans"),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.background = element_rect(fill = "aliceblue", colour = "lightblue"),
    legend.title = element_text(size = 10, face = "bold", colour = "blue4"),
    legend.text = element_text(size = 9, colour = "blue3"),
    strip.background = element_rect(fill = "aliceblue", colour = "lightblue"),
    strip.text = element_text(size = 10, face = "bold", colour = "blue4"),
    legend.position = "right" 
  )
}
attach(data)
# EDA
## BARPLOT DISTRIBUTION OF STUDENTS RESULTS
ggplot(data, aes(x = Target)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, color = "darkblue") +
  labs(title =  "Distribution of student results", x = "Target", y = "Observations") +
  custom_theme()

# 1421 DROPOUT, 794 ENROLLED AND 2209 GRADUATE

data<-data%>%
  filter(Target != "Enrolled")

data$Gender <- factor(data$Gender, levels = c(0, 1), labels = c("Female - 2381", "Male - 1249"))

gender_counts <- data %>%
  count(Gender) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

ggplot(gender_counts, aes(x = "", y = percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Gender Distribution", x = NULL, y = NULL, fill = "Gender") +
  scale_fill_manual(values = c("pink", "skyblue")) +
  custom_theme()

# Distribution of Graduates and Dropouts by `Tuition fees up to date` and Gender graph

data$`Tuition fees up to date` <- factor(data$`Tuition fees up to date`, levels = c(0, 1), labels = c("No", "Yes"))

data_prepared <- data %>%
  group_by(`Tuition fees up to date`, Gender, Target) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(`Tuition fees up to date`) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,  
    Total_Course = sum(Count)              
  )

ggplot(data_prepared, aes(x = `Tuition fees up to date`, y = Percentage, fill = interaction(Target, Gender))) +
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  geom_text(
    aes(
      label = paste0(Count, " (", round(Percentage, 1), "%)")
    ),
    position = position_stack(vjust = 0.5),                   
    size = 3.5,
    color = "white"
  ) +
  labs(
    title = "Distribution of Graduates and Dropouts by `Tuition fees up to date` and Gender",
    x = "`Tuition fees up to date`",
    y = "Percentage (%)",
    fill = "Outcome & Gender"
  ) +
  scale_fill_manual(
    values = c(
      "Graduate.0" = "palevioletred1",      # Female Graduates
      "Graduate.1" = "deepskyblue",        # Male Graduates
      "Dropout.0" = "palevioletred3",      # Female Dropouts
      "Dropout.1" = "dodgerblue4"         # Male Dropouts
    ),
    labels = c(
      "Graduate.0" = "Graduate (Female)",
      "Graduate.1" = "Graduate (Male)",
      "Dropout.0" = "Dropout (Female)",
      "Dropout.1" = "Dropout (Male)"
    )
  ) +
  coord_flip() +  # Orienta i corsi sull'asse Y
  custom_theme() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

#Distribution of Graduates and Dropouts by Displaced and Gender graph
data$Displaced <- factor(data$Displaced, levels = c(0, 1), labels = c("No", "Yes"))

data_prepared <- data %>%
  group_by(Displaced, Gender, Target) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Displaced) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Total_Course = sum(Count)              
  )

ìggplot(data_prepared, aes(x =Displaced, y = Percentage, fill = interaction(Target, Gender))) +
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  geom_text(
    aes(
      label = paste0(Count, " (", round(Percentage, 1), "%)")  
    ),
    position = position_stack(vjust = 0.5),                 
    size = 3.5,
    color = "white"
  ) +
  labs(
    title = "Distribution of Graduates and Dropouts by Displaced and Gender",
    x = "Displaced",
    y = "Percentage (%)",
    fill = "Outcome & Gender"
  ) +
  scale_fill_manual(
    values = c(
      "Graduate.0" = "palevioletred1",      # Female Graduates
      "Graduate.1" = "deepskyblue",        # Male Graduates
      "Dropout.0" = "palevioletred3",      # Female Dropouts
      "Dropout.1" = "dodgerblue4"         # Male Dropouts
    ),
    labels = c(
      "Graduate.0" = "Graduate (Female)",
      "Graduate.1" = "Graduate (Male)",
      "Dropout.0" = "Dropout (Female)",
      "Dropout.1" = "Dropout (Male)"
    )
  ) +
  coord_flip() +  # Orienta i corsi sull'asse Y
  custom_theme() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

#Distribution of Graduates and Dropouts by Debtor and Gender graph

data$Debtor <- factor(data$Debtor, levels = c(0, 1), labels = c("No", "Yes"))

data_prepared <- data %>%
  group_by(Debtor, Gender, Target) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Debtor) %>%
  mutate(
    Percentage = Count / sum(Count) * 100, 
    Total_Course = sum(Count)              
  )

ggplot(data_prepared, aes(x =Debtor, y = Percentage, fill = interaction(Target, Gender))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +  
    aes(
      label = paste0(Count, " (", round(Percentage, 1), "%)")  
    ),
    position = position_stack(vjust = 0.5),                   
    size = 3.5,
    color = "white"
  ) +
  labs(
    title = "Distribution of Graduates and Dropouts by Debtor and Gender",
    x = "Debtor",
    y = "Percentage (%)",
    fill = "Outcome & Gender"
  ) +
  scale_fill_manual(
    values = c(
      "Graduate.0" = "palevioletred1",      # Female Graduates
      "Graduate.1" = "deepskyblue",        # Male Graduates
      "Dropout.0" = "palevioletred3",      # Female Dropouts
      "Dropout.1" = "dodgerblue4"         # Male Dropouts
    ),
    labels = c(
      "Graduate.0" = "Graduate (Female)",
      "Graduate.1" = "Graduate (Male)",
      "Dropout.0" = "Dropout (Female)",
      "Dropout.1" = "Dropout (Male)"
    )
  ) +
  coord_flip() +  
  custom_theme() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

# BARPLOT OF GENDER DISTRIBUTION BY OUTCOME
gender_target_counts <- data %>%
  group_by(Gender, Target) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

ggplot(gender_target_counts, aes(x = Gender, y = count, fill = Target)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Gender distribution by target",
    x = "Gender",
    y = "Number of observations",
    fill = "Target"
  ) +
  scale_x_discrete(labels = c("Female", "Male")) +
  custom_theme()

#"Percentage of Dropout and Graduate by Course" graph
course_labels <- c(
  "1" = "Biofuel Production Technologies",
  "2" = "Animation and Multimedia Design",
  "3" = "Social Service (evening attendance)",
  "4" = "Agronomy",
  "5" = "Communication Design",
  "6" = "Veterinary Nursing",
  "7" = "Informatics Engineering",
  "8" = "Equiniculture",
  "9" = "Management",
  "10" = "Social Service",
  "11" = "Tourism",
  "12" = "Nursing",
  "13" = "Oral Hygiene",
  "14" = "Advertising and Marketing Management",
  "15" = "Journalism and Communication",
  "16" = "Basic Education",
  "17" = "Management (evening attendance)"
)

data$Course <- factor(data$Course, levels = names(course_labels), labels = course_labels)

data_summary <- data %>%
  group_by(Course, Target) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Course) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)  

ggplot(data_summary, aes(x = Course, y = Percentage, fill = Target)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),  
    position = position_dodge(width = 0.9),        
    hjust = -0.1                                    
  ) +
  coord_flip() +
  labs(
    title = "Percentage of Dropout and Graduate by Course",
    y = "Percentage (%)",
    x = "Course"
  ) +
  scale_fill_discrete(name = "Target") +
  theme(legend.position = "bottom") +
  custom_theme()

#"Distribution of Graduates and Dropouts by Course and Gender"
data_prepared <- data %>%
  group_by(Course, Gender, Target) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Course) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,  
    Total_Course = sum(Count)              
  )

ggplot(data_prepared, aes(x = Course, y = Percentage, fill = interaction(Target, Gender))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +   
  geom_text(
    aes(
      label = paste0(Count, " (", round(Percentage, 1), "%)")  
    ),
    position = position_stack(vjust = 0.5),                   
    size = 3.5,
    color = "white"
  ) +
  labs(
    title = "Distribution of Graduates and Dropouts by Course and Gender",
    x = "Course",
    y = "Percentage (%)",
    fill = "Outcome & Gender"
  ) +
  scale_fill_manual(
    values = c(
      "Graduate.0" = "palevioletred1",      # Female Graduates
      "Graduate.1" = "deepskyblue",      # Male Graduates
      "Dropout.0" = "palevioletred3",       # Female Dropouts
      "Dropout.1" = "dodgerblue4"           # Male Dropouts
    ),
    labels = c(
      "Graduate.0" = "Graduate (Female)",
      "Graduate.1" = "Graduate (Male)",
      "Dropout.0" = "Dropout (Female)",
      "Dropout.1" = "Dropout (Male)"
    )
  ) +
  coord_flip() +  
  custom_theme() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"

#"Proportion of Target (Graduate vs Dropout) by Previous Qualification" graph
    previous_qualification_labels <- c(
      "Secondary education", 
      "Higher education—bachelor’s degree",
      "Higher education—degree", 
      "Higher education—master’s degree", 
      "Higher education—doctorate", 
      "Frequency of higher education", 
      "12th year of schooling—not completed", 
      "11th year of schooling—not completed", 
      "Other—11th year of schooling", 
      "10th year of schooling", 
      "10th year of schooling—not completed", 
      "Basic education 3rd cycle (9th/10th/11th year) or equivalent", 
      "Basic education 2nd cycle (6th/7th/8th year) or equivalent", 
      "Technological specialization course", 
      "Higher education—degree (1st cycle)", 
      "Professional higher technical course", 
      "Higher education—master’s degree (2nd cycle)"
    )
    
    data$`Previous qualification` <- factor(data$`Previous qualification`, labels = previous_qualification_labels)
    data$Target <- factor(data$Target, levels = c("Graduate", "Dropout"))
    
    
    data_prepared <- data %>%
      group_by(`Previous qualification`, Target) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(`Previous qualification`) %>%
      mutate(Percentage = Count / sum(Count) * 100)  
    
    ggplot(data_prepared, aes(x = `Previous qualification`, y = Percentage, fill = Target)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +  
      geom_text(
        aes(
          label = paste0(Count, " (", round(Percentage, 1), "%)")  
        ),
        position = position_stack(vjust = 0.5), 
        size = 3.5,
        color = "white"
      ) +
      labs(
        title = "Proportion of Target (Graduate vs Dropout) by Previous Qualification",
        x = "Previous Qualification",
        y = "Percentage"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(size = 10),              
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 10)    
      ) +
      scale_fill_manual(values = c("Graduate" = "#1f77b4", "Dropout" = "#ff7f0e"),  
                        name = "Target Status",  
                        labels = c("Graduates", "Dropouts"))  

    # riconvertiamo i valori della colonna gender in Male e Female
    data$Gender <- factor(data$Gender, levels = c(0, 1), labels = c("Female", "Male"))
    ## AGE GRAPH
    data_prepared <- data %>%
      mutate(Age_Group = cut(`Age at enrollment`, breaks = c(seq(15, 50, by = 5), 70))) %>%
      group_by(Gender, Age_Group, Target) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Gender, Age_Group) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    ggplot(data_prepared, aes(x = Age_Group, y = Percentage, fill = Target)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      labs(
        title = "Distribution of percentages by age, outcome and gender",
        x = "Group of age",
        y = "Percentage",
        fill = "Outcome"
      ) +
      scale_x_discrete(labels = c(
        "(15,20]" = "15-20",
        "(20,25]" = "20-25",
        "(25,30]" = "25-30",
        "(30,35]" = "30-35",
        "(35,40]" = "35-40",
        "(40,45]" = "40-45",
        "(45,50]" = "45-50",
        "(50,70]" = ">50")) +
      guides(fill = guide_legend(title = "Outcome")) +
      facet_wrap(~ Gender, labeller = as_labeller(c("0" = "Female", "1" = "Male"))) +
      custom_theme()

    # HEATMAP OF THE CORRELATIONS
    library(reshape2)
    cor_data <- cor(data[sapply(data, is.numeric)], use = "complete.obs")
    cor_melt <- melt(cor_data)
    
    ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Correlation map",
           x = "Variable",
           y = "Variable",
           fill = "Correlation")
    #focus
    Calcola la matrice di correlazione
    cor_data <- cor(data[sapply(data, is.numeric)], use = "complete.obs")
    cor_melt <- melt(cor_data)
    
    cor_filtered <- cor_melt %>%
      filter(abs(value) > 0.7 & Var1 != Var2)
    
    ggplot(cor_filtered, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Mappa delle correlazioni tra variabili numeriche",
           x = "Variabile",
           y = "Variabile",
           fill = "Correlazione")
    
    print(cor_filtered)
    
    # inizio analisi modelli
    data_filtered <- data %>% 
      filter(Target %in% c("0", "1")) %>%
      mutate(Target = ifelse(Target == "1", 1, 0)) 
    
    
    table(data_filtered$Target)
    
    set.seed(103)
    train_index <- createDataPartition(data_filtered$Target, p = 0.7, list = FALSE)
    train_data <- data_filtered[train_index, ]
    test_data <- data_filtered[-train_index, ]
    
    logistic_model <- glm(Target ~ ., data = train_data, family = binomial())
    summary(logistic_model)
    
    correlate
    
    # analisi senza le correlate
    
    new_log <- glm(Target ~ `Gender` + `Age at enrollment` + `Tuition fees up to date`+ `Scholarship holder`+ 
                     `Debtor`+ `Application mode` + `Curricular units 1st sem (grade)` +
                     `Curricular units 2nd sem (grade)` + Course, 
                   data = balanced_dataset, family = binomial())
    
    summary(new_log)
    BIC(new_log)
    #modello previsione
    set.seed(67)
    train_index <- sample(1:nrow(balanced_dataset), 0.7 * nrow(balanced_dataset))
    train_data <- balanced_dataset[train_index, ]
    test_data <- balanced_dataset[-train_index, ]
    
    new_logP <- glm(Target ~ `Gender` + `Age at enrollment` + `Tuition fees up to date`+ `Scholarship holder`+ 
                      `Debtor`+ `Application mode` + `Curricular units 1st sem (grade)` +
                      `Curricular units 2nd sem (grade)` + Course, 
                    data = train_data, family = binomial())
    
    bic_value <- BIC(new_logP)
    
    predictions <- predict(new_logP, newdata = test_data, type = "response")
    predicted_classes <- ifelse(predictions > 0.5, 1, 0)
    confusion_matrix <- table(test_data$Target, predicted_classes)
    confusion_matrix
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    precision <- confusion_matrix[2,2] / sum(confusion_matrix[,2])
    recall <- confusion_matrix[2,2] / sum(confusion_matrix[2,])
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    library(pROC)
    roc_curve <- roc(test_data$Target, predictions)
    auc_value <- auc(roc_curve)
    cat("Confusion Matrix:\n")
    print(confusion_matrix)
    cat(sprintf("Accuracy: %.2f\n", accuracy))
    cat(sprintf("Precision: %.2f\n", precision))
    cat(sprintf("Recall: %.2f\n", recall))
    cat(sprintf("F1-Score: %.2f\n", f1_score))
    cat(sprintf("AUC-ROC: %.2f\n", auc_value))
    
    plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
    
    #Creazione del modello di regressione logistica
    new_log <- glm(Target ~ `Gender` + `Age at enrollment` + `Tuition fees up to date`+ `Scholarship holder`+ 
                     `Debtor`+ `Application mode` + `Curricular units 1st sem (grade)` +
                     `Curricular units 2nd sem (grade)` + Course, 
                   data = balanced_dataset, family = binomial())
    
    # Valutazione del modello utilizzando il BIC
    aic_value <- AIC(new_log)
    bic_value <- BIC(new_log)
    
    # Stampa del valore BIC
    print(bic_value)
    
    predictions <- predict(new_log, newdata = test_data, type = "response")
    predicted_classes <- ifelse(predictions > 0.5, 1, 0)
    confusion_matrix <- table(test_data$Target, predicted_classes)
    confusion_matrix
    
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    precision <- confusion_matrix[2,2] / sum(confusion_matrix[,2])
    recall <- confusion_matrix[2,2] / sum(confusion_matrix[2,])
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    # Calcolo ROC-AUC
    library(pROC)
    roc_curve <- roc(test_data$Target, predictions)
    auc_value <- auc(roc_curve)
    
    # Stampare i risultati
    cat("Confusion Matrix:\n")
    print(confusion_matrix)
    cat(sprintf("Accuracy: %.2f\n", accuracy))
    cat(sprintf("Precision: %.2f\n", precision))
    cat(sprintf("Recall: %.2f\n", recall))
    cat(sprintf("F1-Score: %.2f\n", f1_score))
    cat(sprintf("AUC-ROC: %.2f\n", auc_value))
    
        
    table(data$Target)
    
    class_distribution <- table(data$Target)
    print(class_distribution)
    
    if (class_distribution[1] != class_distribution[2]) {
      target_0 <- data %>% filter(Target == 0)
      target_1 <- data %>% filter(Target == 1)
      
      set.seed(42)
      if (nrow(target_0) > nrow(target_1)) {
        target_0 <- target_0 %>% sample_n(nrow(target_1))
      } else {
        target_1 <- target_1 %>% sample_n(nrow(target_0))
      }
      
      # Creare il dataset bilanciato
      balanced_dataset <- bind_rows(target_0, target_1)
    } else {
      # Se il dataset è già bilanciato
      balanced_dataset <- data
    }
    
    # Controllare nuovamente il bilanciamento
    table(balanced_dataset$Target)
    
    # Applicare il modello di regressione logistica sui dati bilanciat
    model_balanced <- glm(Target ~ `Gender` + `Age at enrollment`+ `Tuition fees up to date`+ `Scholarship holder`+ 
                            `Unemployment rate`+ `Inflation rate`+ `GDP`+ `Application mode`+
                            `Application order`+ `Daytime/evening attendance`+ `Previous qualification`+ 
                            `Father's qualification`+ `Mother's occupation`+ `Father's occupation`+
                            `Mother's qualification`+ `Curricular units 1st sem (grade)` + `Curricular units 1st sem (without evaluations)` +
                            `Displaced`+ `Educational special needs`+ `Debtor`+ `Marital status`+ Course +
                            `Curricular units 2nd sem (grade)`+ `Curricular units 2nd sem (without evaluations)`, 
                          data = balanced_dataset, family = "binomial")
    
    # Riepilogo del modello
    summary(model_balanced)
    
    # Calcolare l'AIC
    AIC_value <- AIC(model_balanced)
    print(AIC_value)
    
    step_model <- step(model_balanced, direction = "both")
    summary(step_model)
    AIC(step_model)
 
    ####  mixed selection
    set.seed(123)
    train_index <- sample(1:nrow(balanced_dataset), 0.7 * nrow(balanced_dataset))
    train_data <- balanced_dataset[train_index, ]
    test_data <- balanced_dataset[-train_index, ]
    
    model_train <- glm(formula = Target ~ Gender + `Age at enrollment` + `Tuition fees up to date` + 
                         `Scholarship holder` + `Inflation rate` + `Application mode` + 
                         `Daytime/evening attendance` + `Mother's occupation` + `Mother's qualification` + 
                         `Curricular units 1st sem (grade)` + Debtor + Course + `Curricular units 2nd sem (grade)` + 
                         `Curricular units 2nd sem (without evaluations)`, family = "binomial", 
                       data = balanced_dataset)
    
    bic_value <- BIC(model_train)
    
    predictions <- predict(model_train, newdata = test_data, type = "response")
    predicted_classes <- ifelse(predictions > 0.5, 1, 0)
    confusion_matrix <- table(test_data$Target, predicted_classes)
    confusion_matrix
    
    
    training_predictions <- predict(new_log, newdata = train_data, type = "response")
    training_predicted_classes <- ifelse(training_predictions > 0.5, 1, 0)
    
    # Creazione della confusion matrix per il training set
    training_confusion_matrix <- table(train_data$Target, training_predicted_classes)
    
    # Calcolo dell'errore di training
    training_error <- 1 - sum(diag(training_confusion_matrix)) / sum(training_confusion_matrix)
    print(paste("Errore di training:", round(training_error, 4)))
    
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    precision <- confusion_matrix[2,2] / sum(confusion_matrix[,2])
    recall <- confusion_matrix[2,2] / sum(confusion_matrix[2,])
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    # Calcolo ROC-AUC
    library(pROC)
    roc_curve <- roc(test_data$Target, predictions)
    auc_value <- auc(roc_curve)
    
    # Stampare i risultati
    cat("Confusion Matrix:\n")
    print(confusion_matrix)
    cat(sprintf("Accuracy: %.2f\n", accuracy))
    cat(sprintf("Precision: %.2f\n", precision))
    cat(sprintf("Recall: %.2f\n", recall))
    cat(sprintf("F1-Score: %.2f\n", f1_score))
    cat(sprintf("AUC-ROC: %.2f\n", auc_value))
    
    # Plot della curva ROC
    plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
    
    train_preds <- predict(new_log, train_data, type = "response")  # Previsioni per il training
    test_preds <- predict(new_log, test_data, type = "response")  # Previsioni per il test
    
    # 4. Converto le probabilità in classi (0 o 1) utilizzando una soglia di 0.5
    train_preds_class <- ifelse(train_preds > 0.5, 1, 0)
    test_preds_class <- ifelse(test_preds > 0.5, 1, 0)
    
    # 5. Calcolo dell'errore di classificazione
    train_error <- mean(train_preds_class != train_data$Target)  # Errore di training
    test_error <- mean(test_preds_class != test_data$Target)  # Errore di test
    
    # 6. Stampa dell'errore
    cat("Errore di training:", train_error, "\n")
    cat("Errore di test:", test_error, "\n")
    
    # Rifit modello con variabili significative (Pr(>|z|) < 0.05)
    significant_vars <- summary(logistic_model)$coefficients[,4] < 0.05
    reduced_formula <- as.formula(
      paste("Target ~", paste(names(significant_vars)[-1][significant_vars[-1]], collapse = " + "))
    )
    
    log_model_reduced <- glm(Target ~ Course + Nacionality + Displaced + Debtor + 
                               `Tuition fees up to date` + Gender + 
                               `Scholarship holder` + International + 
                               `Curricular units 1st sem (credited)` + 
                               `Curricular units 1st sem (approved)` + 
                               `Curricular units 2nd sem (enrolled)` + 
                               `Curricular units 2nd sem (approved)`,
                             family = binomial(), data = train_data)
    
    summary(log_model_reduced)
    
    # Carica il pacchetto necessario
    library(car)
    
    vif(log_model_reduced)
    
    log_model_reduced <- glm(Target ~ Debtor + 
                               `Tuition fees up to date` + Gender + 
                               `Curricular units 1st sem (credited)` +
                               `Scholarship holder`,
                             family = binomial(), data = train_data)
    
    summary(log_model_reduced)
    
    initial_model <- glm(Target ~ ., family = binomial(), data = train_data)
    
    stepwise_model <- step(initial_model, direction = "both", trace = 1)
    
    summary(stepwise_model)
    
    vif(stepwise_model)
    
    # Rimuovere le variabili collineari e rifare il modello
    model_reduced <- glm(formula = Target ~ Course + `Previous qualification` + 
                           `Mother's occupation` + Displaced + Debtor + 
                           `Tuition fees up to date` + Gender + 
                           `Scholarship holder` + `Age at enrollment` + 
                           `Curricular units 2nd sem (without evaluations)` +
                           `Curricular units 1st sem (credited)` +
                           `Unemployment rate`, 
                         family = binomial(), data = train_data)
    
    vif_model_reduced <- vif(model_reduced)
    vif_model_reduced
    
    summary(model_reduced)
    
    train_data$Total_Credits <- train_data$`Curricular units 1st sem (credited)` + 
      train_data$`Curricular units 1st sem (approved)` + 
      train_data$`Curricular units 2nd sem (enrolled)` + 
      train_data$`Curricular units 2nd sem (approved)`
    
    # Rifare il modello con la variabile aggregata
    model_aggregated <- glm(formula = Target ~ Course + 
                              `Mother's occupation` + Debtor + 
                              `Tuition fees up to date` + Gender + 
                              `Scholarship holder` + `Age at enrollment` + 
                              International + Total_Credits, 
                            family = binomial(), data = train_data)
    
    vif_model_aggregated <- vif(model_aggregated)
    
    vif_model_aggregated
    summary(model_aggregated)
    
    
    

    enrolled_data <- data[data$Target == "Enrolled", ]
    enrolled_predictions <- predict(new_log, newdata = enrolled_data, type = "response")
    
    enrolled_classes <- ifelse(enrolled_predictions > 0.5, "Graduate", "Dropout")
    
    enrolled_data$Predicted_Class <- enrolled_classes
    enrolled_data$Predicted_Probability <- enrolled_predictions
    
    table(enrolled_classes)
    (enrolled_data[, c("Predicted_Class", "Predicted_Probability")])
    
    enrolled_data$Predicted_Probability <- enrolled_predictions
    enrolled_data$Predicted_Class <- enrolled_classes
    
    enrolled_data <- enrolled_data[order(enrolled_data$Predicted_Probability), ]
    
    ggplot(enrolled_data, aes(x = Predicted_Probability, fill = Predicted_Class)) +
      geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("Graduate" = "blue", "Dropout" = "red")) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "black") +
      labs(title = "Distribution of Predicted Probabilities for Enrolled Students",
           x = "Predicted Probability of Graduating",
           y = "Number of Students",
           fill = "Predicted Class") +
      theme_minimal()
    
    