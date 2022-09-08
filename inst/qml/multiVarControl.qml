import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
    VariablesForm
    {
        AvailableVariablesList	{ name: "allVariablesList" }
        AssignedVariablesList	{ name: "variables";	title: qsTr("Control Variables");	suggestedColumns: ["scale"]}
    }






    IntegerField
    {
        name: "previousDataPoints"
        Layout.columnSpan: 2
        label: qsTr("Only show previous")
        defaultValue: 10000
        afterLabel: "data points"
    }


    CheckBox
    {
        name: "overallControlSummaryTable"
        Layout.columnSpan: 2
        label: qsTr("Overall control summary table")
        checked: true
        CheckBox{name : "transposeOverallTable"; label: qsTr("Transpose table")}
        CheckBox{name: "orderTableByOutBound"; label: qsTr("Order table by error out-of-bound proportion")}

    }





    CheckBox
    {
        name: "outBoundOverallPlotCheck"
        label: qsTr("Overall control plot")

        checked: true
        //CheckBox{name: "summaryPlotIndividualVars"; label: qsTr("plot individual variables")}

        RadioButtonGroup
            	{
                	name: "outBoundOverallPlotLineType"
                	radioButtonsOnSameRow: true
                	RadioButton { value: "points";	label: qsTr("Points") }
                	RadioButton { value: "line";	label: qsTr("Line") }
                	RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            	}
        RadioButtonGroup
        {

            name: "outBoundOverallPlotMetricChoice"
            title: qsTr("Out-of-control metric")
            radioButtonsOnSameRow: false
            RadioButton{value: "number"; label: qsTr("Number");checked: true}
            RadioButton{value: "percent"; label: qsTr("Percent")}
        }

    }
    Section
    {
        title: qsTr("Multivariate Binomial Control")

        IntegerField
        {
            name: "multiBinWindow"
            min: 1
            defaultValue: 1
            label: qsTr("Summarise after every")
            afterLabel: qsTr("data points")
        }
        IntegerField{name: multiBinomDraws; defaultValue: 500}

        CheckBox
        {
            name: "multiBinaryCheckPlot"
            title: qsTr("Multivariate binary control plot")
        }
    }
}
