# Logistic-Regression-in-R-applied-to-university-dropout-risk-prediction
This project aims to identify the key factors contributing to university drop-out (attrition) and to predict a student's likelihood of graduation using statistical modeling.

## 🧠 Methodology

We implemented a **Logistic Regression** model for binary classification (Graduate vs. Dropout) based on a dataset of **4,424 student records**, encompassing socio-economic, academic, and demographic variables.

### Key Steps:
1.  **Data Pre-processing:** The initial dataset was filtered to remove 'Enrolled' students and then **balanced** using down-sampling (1421 observations per class) to address class imbalance.
2.  **Feature Selection:** Variables were refined by analyzing correlation matrices and **VIF scores** (Variance Inflation Factor) to mitigate multicollinearity, followed by automated and manual stepwise selection.
3.  **Model Training:** The final model was trained on a 70/30 split of the balanced dataset.

## ✅ Results & Performance

The model successfully identified high-impact variables and demonstrated robust predictive capability:

| Metric | Value | Interpretation |
| :--- | :--- | :--- |
| **Accuracy** | 79\% | Correctly classifying 4 out of 5 students. |
| **AUC-ROC** | 87\% | Strong discrimination ability between the two classes. |
| **Recall** | 81\% | High ability to correctly identify true Dropouts. |

### Key Findings (Driver Analysis)
The interpretation of the model coefficients highlighted the critical influence of economic support:

* **Positive Drivers (Increased Graduation Likelihood):** Being a **Scholarship Holder** and having **Tuition fees up to date**.
* **Negative Drivers (Increased Drop-out Risk):** Being registered as a **Debtor**.

## 🚀 Impact & Application

The model can be used to **estimate the graduation probability** for currently enrolled students, providing actionable insights for university administration. The findings strongly support the development of **targeted economic aid programs** and early academic monitoring to improve retention rates.

---
