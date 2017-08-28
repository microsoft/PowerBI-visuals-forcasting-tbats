/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
module powerbi.extensibility.visual {
    "use strict";
    // below is a snippet of a definition for an object which will contain the property values
    // selected by the users
    /*interface VisualSettings {
        lineColor: string;
    }*/

    // to allow this scenario you should first the following JSON definition to the capabilities.json file
    // under the "objects" property:
    // "settings": {
    //     "displayName": "Visual Settings",
    //     "description": "Visual Settings Tooltip",
    //     "properties": {
    //         "lineColor": {
    //         "displayName": "Line Color",
    //         "type": { "fill": { "solid": { "color": true }}}
    //         }
    //     }
    // }

    // in order to improve the performance, one can update the <head> only in the initial rendering.
    // set to 'true' if you are using different packages to create the widgets
    const updateHTMLHead: boolean = false;

    const renderVisualUpdateType: number[] = [
        VisualUpdateType.Resize,
        VisualUpdateType.ResizeEnd,
        VisualUpdateType.Resize + VisualUpdateType.ResizeEnd
    ];

// USER 
    interface VisualSettingsForecastPlotParams {
        show: boolean;
        forecastLength: number;
        freq1: number;
        freq2: number;
        targetSeason1: string;
        targetSeason2: string;
    }

    interface VisualSettingsConfParams {
        confInterval1: string;
        confInterval2: string;
    }
    interface VisualGraphParams {
        show: boolean;
        dataCol: string;
        forecastCol: string;
        fittedCol: string;
        percentile: number;
        weight: number;
        showFromTo: string;
        refPointShift: number;
        showInPlotFitted: boolean;
    }
    interface VisualAdditionalParams {
        algModeFast: boolean;
        valuesNonNegative: boolean;
        useParProc: boolean;
    }
    interface VisualInfoParams {
        textSize: number;
        infoTextCol: string;
        numDigitsInfo: string;
        whichInfo: string;

    }
    interface VisualAxesParams {
        showScientificY: boolean;
        textSize: number;
        labelsTextCol: string;
        userFormatX: string;
    }
// USER - (END)


    export class Visual implements IVisual {
        private rootElement: HTMLElement;
        private headNodes: Node[];
        private bodyNodes: Node[];
        private settings: VisualSettings;

// USER 
        private settings_forecastPlot_params: VisualSettingsForecastPlotParams;
        private settings_conf_params: VisualSettingsConfParams;
        private settings_graph_params: VisualGraphParams;
        private settings_additional_params: VisualAdditionalParams;
        private settings_info_params: VisualInfoParams;
        private settings_axes_params: VisualAxesParams;
// USER - (END)

        public constructor(options: VisualConstructorOptions) {
            if (options && options.element) {
                this.rootElement = options.element;
            }
            this.headNodes = [];
            this.bodyNodes = [];

// USER 
            this.settings_forecastPlot_params = <VisualSettingsForecastPlotParams>{
                forecastLength: 500,
                freq1: 1,
                freq2: 1,
                targetSeason1: "none",
                targetSeason2: "none"
            };

            this.settings_conf_params = <VisualSettingsConfParams>{
                confInterval1: "0.5",
                confInterval2: "0.995",
            };

            this.settings_graph_params = <VisualGraphParams>{

                dataCol: "orange",
                forecastCol: "red",
                fittedCol: "green",
                percentile: 40,
                weight: 10,
                showFromTo: "all",
                refPointShift: 0,
                showInPlotFitted: false,


            };

            this.settings_additional_params = <VisualAdditionalParams>{
                valuesNonNegative: false,
                algModeFast: true,
                useParProc: false
            };

            this.settings_info_params = <VisualInfoParams>{
                textSize: 10,
                infoTextCol: "gray50",
                numDigitsInfo: "0",
                whichInfo: "none"
            };
            this.settings_axes_params = <VisualAxesParams>{
                showScientificY: false,
                textSize: 12,
                labelsTextCol: "black",
                userFormatX: "auto"
            };
// USER - (END)
        }

        public update(options: VisualUpdateOptions): void {

            if (!options ||
                !options.type ||
                !options.viewport ||
                !options.dataViews ||
                options.dataViews.length === 0 ||
                !options.dataViews[0]) {
                return;
            }
            const dataView: DataView = options.dataViews[0];
            this.settings = Visual.parseSettings(dataView);
// USER 
            this.updateObjects(dataView.metadata.objects);
// USER - (END)
            let payloadBase64: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                payloadBase64 = dataView.scriptResult.payloadBase64;
            }

            if (renderVisualUpdateType.indexOf(options.type) === -1) {
                if (payloadBase64) {
                    this.injectCodeFromPayload(payloadBase64);
                }
            } else {
                this.onResizing(options.viewport);
            }
        }

        public onResizing(finalViewport: IViewport): void {
            /* add code to handle resizing of the view port */
        }

        private injectCodeFromPayload(payloadBase64: string): void {
            // inject HTML from payload, created in R
            // the code is injected to the 'head' and 'body' sections.
            // if the visual was already rendered, the previous DOM elements are cleared

            ResetInjector();

            if (!payloadBase64) {
                return;
            }

            // create 'virtual' HTML, so parsing is easier
            let el: HTMLHtmlElement = document.createElement("html");
            try {
                el.innerHTML = window.atob(payloadBase64);
            } catch (err) {
                return;
            }

            // if 'updateHTMLHead == false', then the code updates the header data only on the 1st rendering
            // this option allows loading and parsing of large and recurring scripts only once.
            if (updateHTMLHead || this.headNodes.length === 0) {
                while (this.headNodes.length > 0) {
                    let tempNode: Node = this.headNodes.pop();
                    document.head.removeChild(tempNode);
                }
                let headList: NodeListOf<HTMLHeadElement> = el.getElementsByTagName("head");
                if (headList && headList.length > 0) {
                    let head: HTMLHeadElement = headList[0];
                    this.headNodes = ParseElement(head, document.head);
                }
            }

            // update 'body' nodes, under the rootElement
            while (this.bodyNodes.length > 0) {
                let tempNode: Node = this.bodyNodes.pop();
                this.rootElement.removeChild(tempNode);
            }
            let bodyList: NodeListOf<HTMLBodyElement> = el.getElementsByTagName("body");
            if (bodyList && bodyList.length > 0) {
                let body: HTMLBodyElement = bodyList[0];
                this.bodyNodes = ParseElement(body, this.rootElement);
            }

            RunHTMLWidgetRenderer();
        }

        private static parseSettings(dataView: DataView): VisualSettings {
            return VisualSettings.parse(dataView) as VisualSettings;
        }


        //RVIZ_IN_PBI_GUIDE:BEGIN:Added to create HTML-based 
        /**
         * This function gets called by the update function above. You should read the new values of the properties into 
         * your settings object so you can use the new value in the enumerateObjectInstances function below.
         * 
         * Below is a code snippet demonstrating how to expose a single property called "lineColor" from the object called "settings"
         * This object and property should be first defined in the capabilities.json file in the objects section.
         * In this code we get the property value from the objects (and have a default value in case the property is undefined)
         */
        public updateObjects(objects: DataViewObjects) {

            this.settings_forecastPlot_params = <VisualSettingsForecastPlotParams>{
                forecastLength: getValue<number>(objects, 'settings_forecastPlot_params', 'forecastLength', 500),
                freq1: getValue<number>(objects, 'settings_forecastPlot_params', 'freq1', 1),
                freq2: getValue<number>(objects, 'settings_forecastPlot_params', 'freq2', 1),
                targetSeason1: getValue<string>(objects, 'settings_forecastPlot_params', 'targetSeason1',"none"),
                targetSeason2: getValue<string>(objects, 'settings_forecastPlot_params', 'targetSeason2',"none")
            };

           this.settings_conf_params = <VisualSettingsConfParams>{
                confInterval1: getValue<string>(objects, 'settings_conf_params', 'confInterval1', "0.5"),
                confInterval2: getValue<string>(objects, 'settings_conf_params', 'confInterval2', "0.995"),

            }
            this.settings_graph_params = <VisualGraphParams>{
                dataCol: getValue<string>(objects, 'settings_graph_params', 'dataCol', "orange"),
                forecastCol: getValue<string>(objects, 'settings_graph_params', 'forecastCol', "red"),
                fittedCol: getValue<string>(objects, 'settings_graph_params', 'fittedCol', "green"),
                percentile: getValue<number>(objects, 'settings_graph_params', 'percentile', 40),
                weight: getValue<number>(objects, 'settings_graph_params', 'weight', 10),
                showFromTo: getValue<string>(objects, 'settings_graph_params', 'showFromTo', "all"),
                refPointShift: getValue<number>(objects, 'settings_graph_params', 'refPointShift', 0),
                showInPlotFitted: getValue<boolean>(objects, 'settings_graph_params', 'showInPlotFitted', false),


            }
            this.settings_additional_params = <VisualAdditionalParams>{
                algModeFast: getValue<boolean>(objects, 'settings_additional_params', 'algModeFast', true),
                valuesNonNegative: getValue<boolean>(objects, 'settings_additional_params', 'valuesNonNegative', false),
                useParProc: getValue<boolean>(objects, 'settings_additional_params', 'useParProc', false)

            }
            this.settings_info_params = <VisualInfoParams>{

                textSize: getValue<number>(objects, 'settings_info_params', 'textSize', 10),
                infoTextCol: getValue<string>(objects, 'settings_info_params', 'infoTextCol', "gray50"),
                numDigitsInfo: getValue<string>(objects, 'settings_info_params', 'numDigitsInfo', "0"),
                whichInfo: getValue<string>(objects, 'settings_info_params', 'whichInfo', "none"),
                
            }
            this.settings_axes_params = <VisualAxesParams>{
                showScientificY: getValue<boolean>(objects, 'settings_axes_params', 'showScientificY', false),
                textSize: getValue<number>(objects, 'settings_axes_params', 'textSize', 12),
                labelsTextCol: getValue<string>(objects, 'settings_axes_params', 'labelsTextCol', "black"),
                userFormatX: getValue<string>(objects, 'settings_axes_params', 'userFormatX', "auto")
            }


        }
        //RVIZ_IN_PBI_GUIDE:END:Added to create HTML-based 





        /** 
         * This function gets called for each of the objects defined in the capabilities files and allows you to select which of the 
         * objects and properties you want to expose to the users in the property pane.
         * 
         */
        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions):
            VisualObjectInstance[] | VisualObjectInstanceEnumerationObject {


            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {
                case 'settings_forecastPlot_params':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            forecastLength: Math.round(inMinMax(this.settings_forecastPlot_params.forecastLength, 1, 1000000)),
                            targetSeason1: this.settings_forecastPlot_params.targetSeason1,
                            targetSeason2: this.settings_forecastPlot_params.targetSeason2
                        },
                        selector: null
                    });

                    if(this.settings_forecastPlot_params.targetSeason1 === "manual")
                    {
                     objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            freq1: Math.round(inMinMax(this.settings_forecastPlot_params.freq1, 1, 1000000))
                        },
                        selector: null
                    });
                    }
                if(this.settings_forecastPlot_params.targetSeason2 === "manual")
                    {
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            freq2: Math.round(inMinMax(this.settings_forecastPlot_params.freq2, 1, 1000000))
                        },
                        selector: null
                    });
                    }
                    break;

                case 'settings_conf_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            confInterval1: this.settings_conf_params.confInterval1,
                            confInterval2: this.settings_conf_params.confInterval2
                        },
                        selector: null
                    });
                    break;

                case 'settings_graph_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            percentile: this.settings_graph_params.percentile,
                            weight: this.settings_graph_params.weight,
                            dataCol: this.settings_graph_params.dataCol,
                            forecastCol: this.settings_graph_params.forecastCol,
                            showInPlotFitted: this.settings_graph_params.showInPlotFitted
                        }
                    });
                    if (this.settings_graph_params.showInPlotFitted) {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                fittedCol: this.settings_graph_params.fittedCol,//conditioned
                            }
                        });
                    }
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            showFromTo: this.settings_graph_params.showFromTo,
                        }
                    });
                    if (this.settings_graph_params.showFromTo != "all") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                refPointShift: this.settings_graph_params.refPointShift//conditioned
                            },
                            selector: null
                        });
                    }
                    break;

                case 'settings_additional_params':

                    objectEnumeration.push({

                        objectName: objectName,
                        properties: {
                            valuesNonNegative: this.settings_additional_params.valuesNonNegative,
                          //  algModeFast: this.settings_additional_params.algModeFast,
                          //  useParProc: this.settings_additional_params.useParProc

                        },
                        selector: null
                    });

                    break;

                case 'settings_info_params':

                    objectEnumeration.push({

                        objectName: objectName,
                        properties: {
                            whichInfo: this.settings_info_params.whichInfo,
                            textSize: this.settings_info_params.textSize,
                            infoTextCol: this.settings_info_params.infoTextCol,
                            numDigitsInfo: this.settings_info_params.numDigitsInfo
                        },
                        selector: null
                    });

                    break;

                case 'settings_axes_params':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            labelsTextCol: this.settings_axes_params.labelsTextCol,
                            textSize: this.settings_axes_params.textSize,
                            userFormatX: this.settings_axes_params.userFormatX,
                            showScientificY: this.settings_axes_params.showScientificY
                        },
                        selector: null
                    });

                    break;
            };



 return objectEnumeration;
           // return VisualSettings.enumerateObjectInstances(this.settings || VisualSettings.getDefault(), options);
        }
    }
}