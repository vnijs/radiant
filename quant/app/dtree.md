> Create and evaluate a decision tree

To create and evaluate a decision tree first (1) enter the structure of the tree in the input editor or (2) load a tree structure from a file. When you first navigate to the _Decide > Decision tree_ tab you will see an example tree structure. This example was created by Christop Glur, the developer of the [data.tree](https://github.com/gluc/data.tree) library ([example source](https://github.com/gluc/useR15/blob/master/00_data/jennylind.yaml)).

To enter a new structure start by providing a name for the tree. In the example below the name for the decision tree is entered as follow: `name: Jenny Lind`. The next step is to indicate the **type** of the first **node**. Options are `type: decision` or `type: chance`.

In the provided example the first node is a **decision node**. The decision maker has to decide to `Sign with Movie Company` or `Sign with TV Network`. Both options lead to a **chance** node with probabilities and payoffs.

> **Note:** Indentation is critically important when defining a tree structure. Use tabs to create branches as shown in the example. Names for branches **must** be followed by a `:` and information about the branch **must** be indented using a tab.

After providing the name for the decision `Sign with Movie Company` the next line **must** be indented with a tab. In the example, the next line starts the description of a chance node (`type: chance`). There are 3 possibilities in the example: (1) `Small Box Office`, (2) `Medium Box Office`, and (3) `Large Box Office`, each with a probability and a payoff. These are the end-points for one branch of the tree and are often referred to as `terminal nodes` or `leaves` of a tree. All endpoints must have a `payoff` value.

> **Note:** Probabilities for a chance node should sum to one and all probabilities smaller than 1 **must** be entered with a leading zero (i.e., 0.1 will work but .1 will not). If you do enter a probability without a leading zero anyway you will see the following message: `Error: non-numeric argument to a binary operator`. Add leading 0's as needed and the error will go away when you click on the `Calculate` button again.

In the example, the decision option `Sign with TV Network` has the same structure and terminal nodes but different payoffs.

## Rules for decision tree input

1. Always start with a tree name (e.g., "name: My tree")
2. The second line always defines a node type (i.e., "type: chance" or "type: decision")
3. All lines must have a ':'. For node names the ':' ends the line. For all other lines it assigns a value.  Specically, it assigns a name (e.g., "name: My tree"), a node type (e.g., "type: decision"), or a number (e.g., "payoff: 100") 
4. A node type must be followed on the next line by a node name (e.g., "Cancel orders:")
5. Use only letters in node names (i.e., no symbols)
6. The line after a node name must be indented
7. End (or terminal) nodes must have a payoff
8. If linked to a chance node, a terminal node must have a probability (e.g, "p: 0.4") and a payoff

If you run into an error you cannot figure out post a question to the the class Piazza site <a href="https://piazza.com/ucsd/fall2015/mgt403" target="_blank" >piazza.com/ucsd/fall2015/mgt403</a>

After specifying the tree structure in the editor, press the `Calculate` button to see the `Initial` and `Final` decision tree in text format on the right-side of the screen (see screen shot below). The initial tree simply shows the tree structure that was specified, together with the node types, probabilities, and payoffs. The final tree shows the optimal decision strategy determined by `folding-back` the tree. In this case, the optimal decision is to `Sign with Movie Company` because this decision has a higher **Expected Monetary Value (EMV)** or payoff.

![dtree model](figures_quant/dtree_model.png)

For visual representation of the decision tree click the _Plot_ tab. If you already clicked the `Calculate` button in the _Model_ tab you will see a graph of the `Initial` decision tree (see screen shot below). Decision nodes are shown in green and chance nodes in orange. If the tree does not look as you intended/expected please return to the _Model_ tab and edit the tree structure.

![dtree plot initial](figures_quant/dtree_plot_initial.png)

The `Final` decision graph shows the optimal decision determined by `folding-back` the tree. As also shown in the _Model_ tab, the optimal decision is to `Sign with Movie Company` because this decision has a higher **Expected Monetary Value**. Note that the optimal decision at each decision node is indicated by a thickness of the line connecting to the next node.

![dtree plot final](figures_quant/dtree_plot_final.png)

The EMV for the chance node following a decision to `Sign with Movie Company` is:

$$
	0.3 \times 200,000 + 0.6 \times 1,000,000 + 0.1 \times 3000000 = 960,000
$$

The EMV for the chance node following a decision to `Sign with TV Network` is:

$$
	0.3 \times 900,000 + 0.6 \times 900,000 + 0.1 \times 900,000 = 900,000
$$

## Buttons

In the _Model_ tab:

* To see this help file click the <i class="fa fa-question" ></i> icon
* To generate a report about the decision tree in the _R > Report_ tab click the <i class="fa fa-edit" ></i> icon
* To save the tree structure entered into the editor window press the `Save input` button
* To save the text representation of the initial and final tree to a text file click the `Save output` button
* To load a tree structure from a file in `yaml` format use the `Choose File` button

In the _Plot_ tab:

* Click the download icon in the top right of your browser to save either the initial or final plot to a pdf-file
