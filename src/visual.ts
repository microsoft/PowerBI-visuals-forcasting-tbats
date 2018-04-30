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

    export class Visual implements IVisual {
        private rootElement: HTMLElement;
        private headNodes: Node[];
        private bodyNodes: Node[];
        private settings: VisualSettings;

        public constructor(options: VisualConstructorOptions) {
            if (options && options.element) {
                this.rootElement = options.element;
            }
            this.headNodes = [];
            this.bodyNodes = [];
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

        // /** 
        //  * This function gets called for each of the objects defined in the capabilities files and allows you to select which of the 
        //  * objects and properties you want to expose to the users in the property pane.
        //  * 
        //  */
        // public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions):
        //     VisualObjectInstance[] | VisualObjectInstanceEnumerationObject {
        //     return VisualSettings.enumerateObjectInstances(this.settings || VisualSettings.getDefault(), options);
        // }
        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions):
            VisualObjectInstance[] | VisualObjectInstanceEnumerationObject {


            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {
                case 'settings_forecastPlot_params':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            forecastLength: Math.round(inMinMax(this.settings.settings_forecastPlot_params.forecastLength, 1, 1000000)),
                            targetSeason1: this.settings.settings_forecastPlot_params.targetSeason1,
                            targetSeason2: this.settings.settings_forecastPlot_params.targetSeason2
                        },
                        selector: null
                    });

                    if (this.settings.settings_forecastPlot_params.targetSeason1 === "manual") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                freq1: Math.round(inMinMax(this.settings.settings_forecastPlot_params.freq1, 1, 1000000))
                            },
                            selector: null
                        });
                    }
                    if (this.settings.settings_forecastPlot_params.targetSeason2 === "manual") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                freq2: Math.round(inMinMax(this.settings.settings_forecastPlot_params.freq2, 1, 1000000))
                            },
                            selector: null
                        });
                    }
                    break;

                case 'settings_conf_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            confInterval1: this.settings.settings_conf_params.confInterval1,
                            confInterval2: this.settings.settings_conf_params.confInterval2
                        },
                        selector: null
                    });
                    break;

                case 'settings_graph_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            percentile: this.settings.settings_graph_params.percentile,
                            weight: this.settings.settings_graph_params.weight,
                            dataCol: this.settings.settings_graph_params.dataCol,
                            forecastCol: this.settings.settings_graph_params.forecastCol,
                            showInPlotFitted: this.settings.settings_graph_params.showInPlotFitted
                        }
                    });
                    if (this.settings.settings_graph_params.showInPlotFitted) {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                fittedCol: this.settings.settings_graph_params.fittedCol,//conditioned
                            }
                        });
                    }
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            showFromTo: this.settings.settings_graph_params.showFromTo,
                        }
                    });
                    if (this.settings.settings_graph_params.showFromTo != "all") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                refPointShift: this.settings.settings_graph_params.refPointShift//conditioned
                            },
                            selector: null
                        });
                    }
                    break;

                case 'settings_additional_params':

                    objectEnumeration.push({

                        objectName: objectName,
                        properties: {
                            valuesNonNegative: this.settings.settings_additional_params.valuesNonNegative,
                            //  algModeFast: this.settings_additional_params.algModeFast,//full mode
                            //  useParProc: this.settings_additional_params.useParProc // full mode

                        },
                        selector: null
                    });

                    break;

                case 'settings_info_params':

                    objectEnumeration.push({

                        objectName: objectName,
                        properties: {
                            whichInfo: this.settings.settings_info_params.whichInfo,
                            textSize: this.settings.settings_info_params.textSize,
                            infoTextCol: this.settings.settings_info_params.infoTextCol,
                            numDigitsInfo: this.settings.settings_info_params.numDigitsInfo
                        },
                        selector: null
                    });

                    break;

                case 'settings_axes_params':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            labelsTextCol: this.settings.settings_axes_params.labelsTextCol,
                            textSize: this.settings.settings_axes_params.textSize,
                            userFormatX: this.settings.settings_axes_params.userFormatX,
                            showScientificY: this.settings.settings_axes_params.showScientificY
                        },
                        selector: null
                    });

                    break;
                case 'settings_export_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings.settings_export_params.show,
                            limitExportSize: this.settings.settings_export_params.limitExportSize,
                            method: this.settings.settings_export_params.method
                        },
                        selector: null
                    });
                    break;
            };
            // USER - replace this block (END)
            return objectEnumeration;
        }
    }
}