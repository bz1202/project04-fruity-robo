# Steam Game Commercial Success Analysis

An in-depth statistical analysis of over 28,000 Steam games, utilizing Data Science and Machine Learning techniques to decode the underlying patterns of commercial success in the digital marketplace.

## 👥 Team: Fruity Robo
* [Yuxun He](https://sprchoir.github.io/biostat777-intro-Yuxun-He/)
* [Tingjun Chen](https://tjchen020524.github.io/biostat777-intro-tingjun-chen/)
* [Boran Zhang](https://bz1202.github.io/biostat777-intro-BORAN-ZHANG/)

---

## 📖 Project Overview
The video game market is saturated, with thousands of games released annually. This project investigates the connection between economic behavior (price, ownership), user engagement (playtime, reviews), and game content (tags).

**Key Goals:**
1.  **Exploratory Analysis:** Visualize the distribution of prices, ownership, and review scores.
2.  **Clustering:** Use Unsupervised Learning (K-Means) to segment the market into distinct performance tiers.

### Key Findings
* **Retention drives Scale:** Long-term engagement is the strongest predictor of commercial value.
* **Two Paths to Success:** We identified a "Viral Hit" formula (Action-heavy, high mechanics) and a "Premium" formula (Narrative-heavy, high price justification).
* **Review Decoupling:** High review scores are common across all price points and do not strongly correlate with market size.

---

## 🔗 Live Data Products
* **Analysis Website:** [View Full Report & Pipeline](https://sprchoir.github.io/Steam_analysis/)
* **Interactive Dashboard:** [Steam Game Explorer (Shiny App)](https://tjchen020524.shinyapps.io/steam_analysis/)

---

## 🛠 Paradigms & Tech Stack
This project demonstrates the integration of **Functional Programming** and **Machine Learning**.

* **Language:** R
* **Data Collection:** SteamSpy API + Web Scraping
* **Analysis:** `tidyverse` (Data manipulation), `stats` (Clustering/PCA)
* **Visualization:** `ggplot2`
* **Deployment:** Quarto (Static Site) + Shiny (Interactive Dashboard)

---