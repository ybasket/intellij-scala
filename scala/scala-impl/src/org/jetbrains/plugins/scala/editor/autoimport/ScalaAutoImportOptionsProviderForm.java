package org.jetbrains.plugins.scala.editor.autoimport;

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.openapi.application.ApplicationBundle;
import com.intellij.ui.IdeBorderFactory;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;
import com.intellij.uiDesigner.core.Spacer;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

/**
 * @author Alefas
 * @since 24.05.12
 */
@SuppressWarnings(value = "unchecked")
public class ScalaAutoImportOptionsProviderForm {
    private static final String INSERT_IMPORTS_ALWAYS = ApplicationBundle.message("combobox.insert.imports.all");
    private static final String INSERT_IMPORTS_ASK = ApplicationBundle.message("combobox.insert.imports.ask");
    private static final String INSERT_IMPORTS_NONE = ApplicationBundle.message("combobox.insert.imports.none");

    private JPanel panel1;
    private JComboBox importOnPasteComboBox;
    private JCheckBox addUnambiguousImportsOnCheckBox;
    private JCheckBox optimizeImportsOnTheCheckBox;

    public ScalaAutoImportOptionsProviderForm() {
        importOnPasteComboBox.addItem(INSERT_IMPORTS_ALWAYS);
        importOnPasteComboBox.addItem(INSERT_IMPORTS_ASK);
        importOnPasteComboBox.addItem(INSERT_IMPORTS_NONE);
    }

    public boolean isAddUnambiguous() {
        return addUnambiguousImportsOnCheckBox.isSelected();
    }

    public void setAddUnambiguous(boolean addUnambiguous) {
        addUnambiguousImportsOnCheckBox.setSelected(addUnambiguous);
    }

    public boolean isOptimizeImports() {
        return optimizeImportsOnTheCheckBox.isSelected();
    }

    public void setOptimizeImports(boolean optimizeImports) {
        optimizeImportsOnTheCheckBox.setSelected(optimizeImports);
    }

    public int getImportOnPasteOption() {
        if (importOnPasteComboBox.getSelectedItem().equals(INSERT_IMPORTS_ALWAYS)) {
            return CodeInsightSettings.YES;
        } else if (importOnPasteComboBox.getSelectedItem().equals(INSERT_IMPORTS_ASK)) {
            return CodeInsightSettings.ASK;
        } else {
            return CodeInsightSettings.NO;
        }
    }

    public void setImportOnPasteOption(int importOnPasteOption) {
        switch (importOnPasteOption) {
            case CodeInsightSettings.YES:
                importOnPasteComboBox.setSelectedItem(INSERT_IMPORTS_ALWAYS);
                break;
            case CodeInsightSettings.ASK:
                importOnPasteComboBox.setSelectedItem(INSERT_IMPORTS_ASK);
                break;
            case CodeInsightSettings.NO:
                importOnPasteComboBox.setSelectedItem(INSERT_IMPORTS_NONE);
                break;
        }
    }

    public JComponent getComponent() {
        return panel1;
    }

    {
// GUI initializer generated by IntelliJ IDEA GUI Designer
// >>> IMPORTANT!! <<<
// DO NOT EDIT OR ADD ANY CODE HERE!
        $$$setupUI$$$();
    }

    /**
     * Method generated by IntelliJ IDEA GUI Designer
     * >>> IMPORTANT!! <<<
     * DO NOT edit this method OR call it in your code!
     *
     * @noinspection ALL
     */
    private void $$$setupUI$$$() {
        panel1 = new JPanel();
        panel1.setLayout(new GridLayoutManager(4, 3, new Insets(0, 0, 0, 0), -1, -1));
        panel1.putClientProperty("BorderFactoryClass", "com.intellij.ui.IdeBorderFactory$PlainSmallWithIndent");
        panel1.setBorder(IdeBorderFactory.PlainSmallWithIndent.createTitledBorder(BorderFactory.createEtchedBorder(), "Scala", TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null, null));
        final JLabel label1 = new JLabel();
        label1.setText("Insert imports on paste:");
        panel1.add(label1, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        importOnPasteComboBox = new JComboBox();
        panel1.add(importOnPasteComboBox, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final Spacer spacer1 = new Spacer();
        panel1.add(spacer1, new GridConstraints(0, 2, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, 1, null, null, null, 0, false));
        addUnambiguousImportsOnCheckBox = new JCheckBox();
        addUnambiguousImportsOnCheckBox.setText("Add unambiguous imports on the fly");
        panel1.add(addUnambiguousImportsOnCheckBox, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JLabel label2 = new JLabel();
        label2.setText("To disable import popup, use Java setting");
        panel1.add(label2, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        optimizeImportsOnTheCheckBox = new JCheckBox();
        optimizeImportsOnTheCheckBox.setText("Optimize imports on the fly");
        panel1.add(optimizeImportsOnTheCheckBox, new GridConstraints(3, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    }

    /**
     * @noinspection ALL
     */
    public JComponent $$$getRootComponent$$$() {
        return panel1;
    }

}
