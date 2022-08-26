// ------------------------------------------------------------------------------
// com.DPFaragir.listview.DPFListView Java Sub Class
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
package com.DPFaragir.Animation;

import android.animation.ValueAnimator;
import android.view.animation.Transformation;
import android.view.animation.Animation;
import android.view.animation.TranslateAnimation;
import android.view.animation.AlphaAnimation;
import android.view.animation.AnimationSet;
import android.view.animation.RotateAnimation;
import android.view.animation.ScaleAnimation;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.view.Menu;
import android.view.KeyEvent;
import android.widget.LinearLayout;
import android.widget.*;
import android.graphics.Typeface;
import android.graphics.Color;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.*;

//------------------------------------------------------------------------------
// DPFListView
//------------------------------------------------------------------------------
public class DPFAnimation
{

    private Context mContext;
    private View mView;
    private DPFOnAnimationListener mListener;
    private float mFromXDelta;
    private float mToXDelta;
    private float mFromYDelta;
    private float mToYDelta;
    private float mxDelta;
    private float myDelta;
    private float mFromAlpha;
    private float mToAlpha;
    private float mAlpha;


    //--------------------------------------------------------------------------
    // @startTranslateAnimation
    //--------------------------------------------------------------------------
    public void startTranslateAnimation(Context context, View view, float screenScale, float fromXDelta, float toXDelta, float fromYDelta, float toYDelta, float fromAlpha, float toAlpha, int durationMillis, DPFOnAnimationListener listener)
    {
        mContext = context;
        mListener = listener;
        mView = view ;

        mFromXDelta = fromXDelta * screenScale;
        mToXDelta   = toXDelta   * screenScale;
        mFromYDelta = fromYDelta * screenScale;
        mToYDelta   = toYDelta   * screenScale;
        mxDelta     = mToXDelta - mFromXDelta ;
        myDelta     = mToYDelta - mFromYDelta ;
        mFromAlpha  = fromAlpha;
        mToAlpha    = toAlpha;
        mAlpha      = mToAlpha - mFromAlpha ;

        //Toast.makeText(mContext, Float.toString(mFromXDelta), Toast.LENGTH_LONG).show();

        Animation animation = new Animation()
        {
            @Override
            protected void applyTransformation(float interpolatedTime, Transformation t)
            {
                mView.setX( mFromXDelta + mxDelta * interpolatedTime );
                mView.setY( mFromYDelta + myDelta * interpolatedTime );
                mView.setAlpha( mFromAlpha + mAlpha * interpolatedTime );

                //RelativeLayout.LayoutParams params = (RelativeLayout.LayoutParams)mView.getLayoutParams();
                //params.leftMargin = (int)(mToXDelta * interpolatedTime);
                //params.topMargin = (int)(mToYDelta * interpolatedTime);
                //mView.setLayoutParams(params);
            }

        };

        animation.setAnimationListener(new TranslateAnimation.AnimationListener()
        {
                @Override
                public void onAnimationStart(Animation animation)
                {
                    mListener.onAnimationStart(animation);
                }

                @Override
                public void onAnimationRepeat(Animation animation)
                {
                    mListener.onAnimationRepeat(animation);
                }

                @Override
                public void onAnimationEnd(Animation animation)
                {
                    mListener.onAnimationEnd(animation);
                }

        });

        animation.setDuration( durationMillis );
        mView.startAnimation( animation );


    }
    //--------------------------------------------------------------------------

}
