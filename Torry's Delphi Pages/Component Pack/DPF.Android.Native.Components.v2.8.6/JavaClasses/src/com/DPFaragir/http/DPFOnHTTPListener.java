// ------------------------------------------------------------------------------
// com.DPFaragir.http.DPFOnHTTPListener Interface
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
package com.DPFaragir.http;

import android.graphics.Bitmap;
import android.net.http.SslError;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.*;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.view.View;

//------------------------------------------------------------------------------
// DPFOnHTTPListener
//------------------------------------------------------------------------------
public interface DPFOnHTTPListener
{
    public abstract void onCancelled(DPFHTTP DPFHttp);
    public abstract void onStarted(DPFHTTP DPFHttp);
    public abstract void onProgressUpdate(DPFHTTP DPFHttp, Integer progress, Integer downloadSize, Integer downloaded);
    public abstract void onFinished(DPFHTTP DPFHttp, Integer returnCode, Integer httpResponseCode, String httpResponseMessage, String error);
}


