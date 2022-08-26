// ------------------------------------------------------------------------------
// com.DPFaragir.webview.DPFOnWebViewListener Java WebView Listener
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

import android.graphics.Bitmap;
import android.net.http.SslError;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.*;

//------------------------------------------------------------------------------
// DPFOnWebViewListener
//------------------------------------------------------------------------------
public interface DPFOnWebViewListener
{
    public abstract void doUpdateVisitedHistory(WebView webview, String s, boolean flag);
    public abstract void onFormResubmission(WebView webview, Message message, Message message1);
    public abstract void onLoadResource(WebView webview, String s);
    public abstract void onPageFinished(WebView webview, String s);
    public abstract void onPageStarted(WebView webview, String s, Bitmap bitmap);
    public abstract void onReceivedError(WebView webview, int i, String s, String s1);
    public abstract void onReceivedHttpAuthRequest(WebView webview, HttpAuthHandler httpauthhandler, String s, String s1);
    public abstract void onReceivedSslError(WebView webview, SslErrorHandler sslerrorhandler, SslError sslerror);
    public abstract void onScaleChanged(WebView webview, float f, float f1);
    public abstract void onUnhandledKeyEvent(WebView webview, KeyEvent keyevent);
    public abstract boolean shouldOverrideKeyEvent(WebView webview, KeyEvent keyevent);
    public abstract boolean shouldOverrideUrlLoading(WebView webview, String s);
}


