# School_Projects
From Data Engineering to Statistical Insights

This repository showcases a comprehensive, data-driven approach to complex business challenges, integrating **Relational Database Design (SQL)**, **Statistical Modeling (R)**, and **Actionable Business Intelligence**.

---

## Project Structure
The repository is organized into three core analytical pillars:
* **`SQL-Retail-Database/`**: Database architecture and schema design for an electronic sales system.
* **`Turo-Pricing-Analysis/`**: Multiple linear regression analysis of the peer-to-peer car-sharing market.
* **`Telco-Churn-Prediction/`**: Logistic regression and data mining to enhance customer retention strategies.

---

## 1. Electronic Sales: Relational Database Design (SQL)
**Objective:** To architect a robust database schema capable of managing high-volume retail operations and supporting real-time sales analytics.

### Key Implementation:
* **Normalized Schema Design:** Developed a multi-entity database featuring `Customer`, `Store`, `Product`, `Inventory`, and `Sales_Line` tables to maintain 3rd Normal Form (3NF).
* **Data Integrity:** Established strict **Primary Key** and **Foreign Key** relationships to ensure consistency across transactions, shipping, and banking data.

---

## 2. Turo.com: Market Pricing Case Study (R)
**Objective:** To identify the key statistical drivers that influence listing prices on Turo.com, the "Airbnb for cars."

### Methodology & Insights:
* **Exploratory Data Analysis (EDA):** Cleaned the Los Angeles market dataset, handling missing values and removing outliers via the **IQR (Interquartile Range) method**.
* **Statistical Modeling:** Built **Multiple Linear Regression** models in R to quantify the impact of car age (`car.year`), host reputation (`host.all.star`), and delivery convenience on rental rates.
* **Business Impact:** Identified that "All-Star" status and vehicle year are the primary predictors of price elasticity, providing a data-backed pricing strategy for new hosts.

---

## 3. Telco: Customer Churn Prediction (R)
**Objective:** To predict customer turnover probability and identify risk factors to improve retention in the telecommunications industry.

### Methodology & Insights:
* **Predictive Modeling:** Employed **Logistic Regression** and classification techniques to determine the likelihood of a customer terminating their contract.
* **Model Evaluation:** Validated performance using **10-fold cross-validation**, Confusion Matrices, and ROC curves to ensure high predictive accuracy.
* **Managerial Insights:** Discovered that fiber optic users and month-to-month contract holders have the highest churn risk, leading to recommendations for targeted loyalty programs.


---

## Tech Stack
* **Languages:** R (Statistical Computing), SQL (Database Management)
* **Statistical Techniques:** Linear & Logistic Regression, Causal Inference, Data Mining, EDA
* **Tools:** MySQL, RStudio, MS Excel 
* **Key R Libraries:** `tidyverse`, `ggplot2`, `caret`, `dplyr`, `corrplot`

---

## 📫 Contact & Connect
* **Name:** WenYu Chen 
* **Education:** M.S. in Business Analytics (Class of 2026)
* **Specialization:** Supply Chain Management, Procurement, & Data Analytics
* **LinkedIn:** https://www.linkedin.com/in/wenyu-chen-5573401b3
