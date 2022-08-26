// ------------------------------------------------------------------------------
// com.DPFaragir.listview.DPFWebClient Java Sub Class
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
// DPFWebClient
//------------------------------------------------------------------------------
public class DPFWebClient extends WebViewClient
{
    private DPFOnWebViewListener mListener;

    //-----------------------------------------------------
    public DPFWebClient()
    {
       mListener = null;
    }

    public void SetWebViewListener(DPFOnWebViewListener onwebviewlistener)
    {
        mListener = onwebviewlistener;
    }

    //-----------------------------------------------------
   @Override
    public void doUpdateVisitedHistory(WebView webview, String s, boolean flag)
    {
        super.doUpdateVisitedHistory(webview, s, flag);
        if(mListener != null)
            mListener.doUpdateVisitedHistory(webview, s, flag);
    }

    //-----------------------------------------------------
   @Override
    public void onFormResubmission(WebView webview, Message message, Message message1)
    {
        super.onFormResubmission(webview, message, message1);
        if(mListener != null)
            mListener.onFormResubmission(webview, message, message1);
    }

    //-----------------------------------------------------
   @Override
    public void onLoadResource(WebView webview, String s)
    {
        super.onLoadResource(webview, s);
        if(mListener != null)
            mListener.onLoadResource(webview, s);
    }

    //-----------------------------------------------------
   @Override
    public void onPageFinished(WebView webview, String s)
    {
        super.onPageFinished(webview, s);
        if(mListener != null)
            mListener.onPageFinished(webview, s);
    }

    //-----------------------------------------------------
   @Override
    public void onPageStarted(WebView webview, String s, Bitmap bitmap)
    {
        super.onPageStarted(webview, s, bitmap);
        if(mListener != null)
            mListener.onPageStarted(webview, s, bitmap);
    }

    //-----------------------------------------------------
   @Override
    public void onReceivedError(WebView webview, int i, String s, String s1)
    {
        super.onReceivedError(webview, i, s, s1);
        if(mListener != null)
            mListener.onReceivedError(webview, i, s, s1);
    }

    //-----------------------------------------------------
   @Override
    public void onReceivedHttpAuthRequest(WebView webview, HttpAuthHandler httpauthhandler, String s, String s1)
    {
        super.onReceivedHttpAuthRequest(webview, httpauthhandler, s, s1);
        if(mListener != null)
            mListener.onReceivedHttpAuthRequest(webview, httpauthhandler, s, s1);
    }

    //-----------------------------------------------------
   @Override
    public void onReceivedSslError(WebView webview, SslErrorHandler sslerrorhandler, SslError sslerror)
    {
        super.onReceivedSslError(webview, sslerrorhandler, sslerror);
        if(mListener != null)
            mListener.onReceivedSslError(webview, sslerrorhandler, sslerror);
    }

    //-----------------------------------------------------
   @Override
    public void onScaleChanged(WebView webview, float f, float f1)
    {
        super.onScaleChanged(webview, f, f1);
        if(mListener != null)
            mListener.onScaleChanged(webview, f, f1);
    }

    //-----------------------------------------------------
   @Override
    public void onUnhandledKeyEvent(WebView webview, KeyEvent keyevent)
    {
        super.onUnhandledKeyEvent(webview, keyevent);
        if(mListener != null)
            mListener.onUnhandledKeyEvent(webview, keyevent);
    }

    //-----------------------------------------------------
   @Override
    public boolean shouldOverrideKeyEvent(WebView webview, KeyEvent keyevent)
    {
        if(mListener != null)
            return mListener.shouldOverrideKeyEvent(webview, keyevent);
        else
            return super.shouldOverrideKeyEvent(webview, keyevent);
    }

    //-----------------------------------------------------
   @Override
    public boolean shouldOverrideUrlLoading(WebView webview, String s)
    {
        super.shouldOverrideUrlLoading(webview, s);
        webview.loadUrl(s);
        if(mListener != null)
            return mListener.shouldOverrideUrlLoading(webview, s);
        else
            return super.shouldOverrideUrlLoading(webview, s);
    }

}
