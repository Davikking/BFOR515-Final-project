import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier, plot_tree
from sklearn.metrics import classification_report, accuracy_score
import matplotlib.pyplot as plt

# Load dataset
Violation_data = pd.read_csv(r"C:\Users\Christoph\Documents\NY_Violations_score_Grade.csv")

# Ensure that missing data is handled
dViolation_data = Violation_data.dropna(subset=['GRADE', 'TOTAL...CRITICAL.VIOLATIONS', 'SCORE'])

# Select features and target column
features = Violation_data[['TOTAL...CRITICAL.VIOLATIONS', 'SCORE']]  # Feature columns
target = Violation_data['GRADE']  # Target column (what you want to predict)

# Split the data into training and testing sets (80% train, 20% test)
X_train, X_test, y_train, y_test = train_test_split(features, target, test_size=0.2, random_state=42)

# Create and train the Decision Tree model
dtree = DecisionTreeClassifier(random_state=42)
dtree.fit(X_train, y_train)

# Make predictions
y_pred = dtree.predict(X_test)

# Evaluate the model
print("Accuracy:", accuracy_score(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred))
class_names = [str(c) for c in target.unique()]  # Ensures all class names are strings
# Visualize the decision tree (optional)
plt.figure(figsize=(12, 8))
plot_tree(dtree, filled=True, feature_names=features.columns, class_names=class_names, fontsize=10)
plt.title("Decision Tree - GRADE Prediction")
plt.show()
