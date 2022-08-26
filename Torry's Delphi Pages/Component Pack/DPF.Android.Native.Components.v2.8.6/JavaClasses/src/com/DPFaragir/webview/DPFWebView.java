// ------------------------------------------------------------------------------
// com.DPFaragir.listview.DPFWeView Java Sub Class
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
package com.DPFaragir.webview;

import android.content.Context;
import android.graphics.Bitmap;
import android.net.http.SslError;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.*;


//------------------------------------------------------------------------------
// DPFWeView
//------------------------------------------------------------------------------
public class DPFWebView extends WebView {
    private Context mContext;
    private DPFWebClient mWebClient;

    //-----------------------------------------------------
    public DPFWebView(Context context) {
        super(context);
        mContext = context;
        mWebClient = new DPFWebClient();
        setWebViewClient(mWebClient);
    }

    //-----------------------------------------------------
    public void SetWebViewListener(DPFOnWebViewListener onwebviewlistener)
    {
        mWebClient.SetWebViewListener(onwebviewlistener);
    }

    //-----------------------------------------------------
    @Override
    public boolean performLongClick() {
        return true;
    }

}
