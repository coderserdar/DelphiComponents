// ------------------------------------------------------------------------------
// com.DPFaragir.DPFTextView Java Sub Class
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
package com.DPFaragir;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.widget.TextView;
import android.widget.TableLayout;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.graphics.Color;
import android.text.method.ScrollingMovementMethod;

public class DPFTextView extends ScrollView {

    private TextView mTextView ;

    //--------------------------------------------------------------------------
    public DPFTextView(Context context) {
        super(context);
        mTextView = new TextView(context);
        mTextView.setLayoutParams(new TableLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT, 1f));
        addView(mTextView);
    }

    //--------------------------------------------------------------------------
    public DPFTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mTextView = new TextView(context);
        mTextView.setLayoutParams(new TableLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT, 1f));
        addView(mTextView);
    }

    //--------------------------------------------------------------------------
    public DPFTextView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        mTextView = new TextView(context);
        mTextView.setLayoutParams(new TableLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT, 1f));
        addView(mTextView);
    }

    //--------------------------------------------------------------------------
    public void setText(CharSequence text){
       mTextView.setText(text);
    }

    //--------------------------------------------------------------------------
    public void setTextColor(int color){
       mTextView.setTextColor( color );
    }

    //--------------------------------------------------------------------------
    public void setAllCaps( Boolean allCaps){
       mTextView.setAllCaps( allCaps );
    }

    //--------------------------------------------------------------------------
    public void setLines( int lines){
       if(lines > 0 )
         mTextView.setLines( lines );
       else{
          mTextView.setLines( 0 );
          mTextView.setMaxLines(Integer.MAX_VALUE);
       }
    }

    //--------------------------------------------------------------------------
    public void setGravity( int gravity){
       mTextView.setGravity( gravity );
    }

    //--------------------------------------------------------------------------
    public void setTextSize( float  size){
       mTextView.setTextSize( size );
    }

    //--------------------------------------------------------------------------
    public void setTextSize(int unit, float size){
       mTextView.setTextSize( unit, size);
    }

    //--------------------------------------------------------------------------
    protected void onDraw (Canvas canvas) {
        super.onDraw(canvas);


    }

}
