/*
 *  Power BI Visualizations
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

  import DataViewObjectsParser = powerbi.extensibility.utils.dataview.DataViewObjectsParser;

  export function inMinMax(a: number, mi: number, ma: number) {
    if (a < mi)
      return mi;
    if (a > ma)
      return ma;
    return a;
  }
  export class VisualSettings extends DataViewObjectsParser {
    // public rcv_script: rcv_scriptSettings = new rcv_scriptSettings();
    public settings_forecastPlot_params: VisualSettingsForecastPlotParams = new VisualSettingsForecastPlotParams();
    public settings_conf_params: VisualSettingsConfParams = new VisualSettingsConfParams();
    public settings_graph_params: VisualGraphParams = new VisualGraphParams();
    public settings_additional_params: VisualAdditionalParams = new VisualAdditionalParams();
    public settings_info_params: VisualInfoParams = new VisualInfoParams();
    public settings_axes_params: VisualAxesParams = new VisualAxesParams();
    public settings_export_params: VisualSettingsExportParams = new VisualSettingsExportParams();

  }

  export class VisualSettingsForecastPlotParams {
    public forecastLength: number = 500;
    public freq1: number = 1;
    public freq2: number = 1;
    public targetSeason1: string = "none";
    public targetSeason2: string = "none";
  }
  export class VisualSettingsConfParams {
    public confInterval1: string = "0.5";
    public confInterval2: string = "0.995";
  }
  export class VisualGraphParams {
    public dataCol: string = "orange";
    public forecastCol: string = "red";
    public fittedCol: string = "green";
    public percentile: number = 40;
    public weight: number = 10;
    public showFromTo: string = "all";
    public refPointShift: number = 0;
    public showInPlotFitted: boolean = false;
  }
  export class VisualAdditionalParams {
    public algModeFast: boolean = true;
    public valuesNonNegative: boolean = false;
    public useParProc: boolean = false;
  }
  export class VisualInfoParams {
    public textSize: number = 10;
    public infoTextCol: string = "gray50";
    public numDigitsInfo: string = "0";
    public whichInfo: string = "none";
  }
  export class VisualAxesParams {
    public showScientificY: boolean = false;
    public textSize: number = 12;
    public labelsTextCol: string = "black";
    public userFormatX: string = "auto";
  }
  export class VisualSettingsExportParams {
    public show: boolean = false;
    public limitExportSize: string = "10000";
    public method: string = "copy";
  }



}
