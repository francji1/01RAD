# Regression Data Analysis (01RAD)

Materials for the exercises of **01RAD - Regresni analyza dat** at FJFI CTU in Prague, winter semester 2026/27.
Lectures: doc. Tomas Hobza. Exercises: Jiri Franc.

## Repository structure

- **`code/`**
  Jupyter notebooks for the exercises, named `01RAD_ExNN.ipynb`. Notebooks are added weekly as the semester progresses.
  Homework assignments are published as `01RAD_ExNN_HW.ipynb` together with the exercise.

- **`HW/`**
  Homework assignments and submissions. Solve the assignment and open a pull request that adds your notebook as
  `HW/01RAD_ExNN_HW_<Surname>.ipynb`; see `HW/README.md`. A selected student solution follows a week later, with a reference solution when needed.

- **`data/`**
  Datasets used in the notebooks. Load them directly from GitHub, for example
  `https://raw.githubusercontent.com/francji1/01RAD/main/data/fsdata.csv`.

- **`lectures/`**
  Lecture slides (PDF).

## Exercises so far

| Exercise | Topic | Homework |
|---|---|---|
| [Ex00](code/01RAD_Ex00.ipynb) | Working in Python notebooks | |
| [Ex01](code/01RAD_Ex01.ipynb) | Simple linear regression | [HW01](HW/01RAD_Ex01_HW.ipynb) Cars braking distance |
| [Ex02](code/01RAD_Ex02.ipynb) | Inference in simple linear regression | [HW02](HW/01RAD_Ex02_HW.ipynb) Regression to the mean |
| [Ex03](code/01RAD_Ex03.ipynb) | ANOVA table, $R^2$, prediction and residuals | [HW03](HW/01RAD_Ex03_HW.ipynb) Shortleaf pines |
| [Ex04](code/01RAD_Ex04.ipynb) | Multiple linear regression through linear algebra | [HW04](HW/01RAD_Ex04_HW.ipynb) House prices (team work, due 1 November) |

## How to run the notebooks

- Open any notebook in Google Colab via the badge at its top, or
- clone the repository and run locally with Python 3.11+ and

  ```bash
  pip install numpy pandas scipy statsmodels matplotlib seaborn scikit-learn
  ```

## Course overview

1. Simple linear regression: least squares and maximum likelihood estimates, Gauss-Markov theorem, confidence intervals and tests, ANOVA table, prediction.
2. Multiple linear regression: matrix formulation, hat matrix, properties of estimators, general linear hypothesis.
3. Residuals, diagnostics and influence measures.
4. Transformations, Box-Cox, weighted least squares.
5. Model selection: information criteria, stepwise procedures.
6. Collinearity and ridge regression; extensions to robust and regularised regression.

Materials from previous years are available in the git history.
