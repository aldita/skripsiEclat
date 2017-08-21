----
<h5>This page for helping you using FAST for doing Apriori Analysis</h5>
----

Frequent item set mining and association rule induction [Agrawal and Srikant 1994] are powerful methods for so-called market basket analysis, which aims at finding regularities in the shopping behavior of customers of supermarkets, mail-order companies, online shops etc. With the induction of frequent item sets and association rules one tries to find sets of products that are frequently bought together, so that from the presence of certain products in a shopping cart one can infer (with a high probability) that certain other products are present. Such information, especially if expressed in the form of rules, can often be used to increase the number of items sold, for instance, by appropriately arranging the products on the shelves of a supermarket or on the pages of a mail-order catalog (they may, for example, be placed adjacent to each other in order to invite even more customers to buy them together) or by directly suggesting items to a customer, which may be of interest for him/her.

An association rule is a rule like "If a customer buys wine and bread, he/she often buys cheese, too." It expresses an association between (sets of) items, which may be products of a supermarket or a mail-order company, special equipment options of a car, optional services offered by telecommunication companies etc. An association rule states that if we pick a customer at random and find out that he/she selected certain items (bought certain products, chose certain options etc.), we can be confident, quantified by a percentage, that he/she also selected certain other items (bought certain other products, chose certain other options etc.).
</br>
</br>
The step of using this Analysis are </br>
1. Select data. Determine the data for each column. If you want to use all of columns, choose 'select all' and otherwise if you want choose one by one. It has three type of data. They are table for transaction each column, transaction each row, and transaction needed convert from single item to basket item. User can remove duplicated too </br>
2. Convert to data transaction by choosing TRANSACTIONS DATA </br>
3. Open select parameter then choose a appropriate parameter. then click tab summary. User can see the frequency of each item </br>
4. Open the rules for getting rules of apriori</br>

Support is percentage of the transactions which contain that items. Range: [0, 1]</br>
Confidence is support of the set of all items that appear in the rule divided by the support of the left hand side of the rule (here X).Range: [0, 1]</br>
Lift is a corelation measure. It has no corelation when the value 1. Range: [0, ~]</br>
Chi Squared is a corelation measure and suitable for small dataset. Positive corelation when >3.84. Range: [0, ~]</br>
Kulzcynski is alternative measure because it considers null invariant. Range: [0, 1]</br>
Imbalance Ratio is to knowing wether rule balance or not. Range: [0, 1]</br>

5. User choose adjust plot then open the plot </br>
6. user can see the itemfrequency, and other plot of rules </br>
7. User generate report and share to forum by clicking GENERATE REPORT sidebar</br>