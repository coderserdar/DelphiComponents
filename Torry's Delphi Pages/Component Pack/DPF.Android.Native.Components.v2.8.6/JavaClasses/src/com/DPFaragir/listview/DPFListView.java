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
package com.DPFaragir.listview;

import android.content.Context;
import android.os.Bundle;
import android.app.ListActivity;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.view.Gravity;
import android.view.ViewGroup;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.KeyEvent;
import android.widget.LinearLayout;
import android.widget.*;
import android.widget.AbsListView.*;
import android.graphics.Typeface;
import android.graphics.Color;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.*;

//------------------------------------------------------------------------------
// DPFListView
//------------------------------------------------------------------------------
public class DPFListView extends ListView {
    private DPFListView mDPFListView;
    private Context mContext;
    private DPFOnListViewListener mListener;
    //private int mSelectedItem ;
    private StableArrayAdapter mAdapter ;
    private static View customView;
    private static int mSelectedColor = android.R.drawable.list_selector_background;
    private ArrayList<Integer> mSelectedItems = new ArrayList <Integer> ();
    private int mListChoiceMode = ListView.CHOICE_MODE_SINGLE;

    public class Station {

       public String text1;
       public String text2;
    }
    private class StableArrayAdapter extends ArrayAdapter<Station> {

            HashMap<Station, Integer> mIdMap = new HashMap<Station, Integer>();
            private int CellType ;

            public StableArrayAdapter(Context context, int textViewResourceId, List<Station> objects) {
                 super(context, textViewResourceId, objects);
                 CellType = textViewResourceId ;
                 for (int i = 0; i < objects.size(); ++i) {
                    mIdMap.put(objects.get(i), i);
                 }
            }

    //-----------------------------------------------------
    // Make simple_list_item_2 View For List View Cell
    //-----------------------------------------------------
    private View getListItem2View(int position, View convertView, ViewGroup parent) {

        //final View renderer = super.getView(position, convertView, parent);
        //convertView = renderer;
        //((TextView) convertView).setTextColor(Color.BLACK);
        //Under code is for custom view !
        TextView tv1 = null;
        TextView tv2 = null;
        if (convertView == null) {
            LinearLayout lL = new LinearLayout(mContext);
            lL.setLayoutParams(new android.view.ViewGroup.LayoutParams(android.view.ViewGroup.LayoutParams.MATCH_PARENT,android.view.ViewGroup.LayoutParams.MATCH_PARENT));
            lL.setOrientation(LinearLayout.VERTICAL);
            lL.setGravity(Gravity.BOTTOM);
            lL.setPadding(10, 10, 10, 10);

            tv1 = new TextView(mContext);
            tv1.setTextColor(Color.BLACK);
            tv1.setId(101010);
            tv1.setText("DPF");

            tv2 = new TextView(mContext);
            tv2.setTextColor(Color.BLACK);
            tv2.setId(101011);
            tv2.setText("DPF");

            lL.addView(tv1);
            lL.addView(tv2);
            convertView = lL;
        }
        else {
            tv1 = (TextView) convertView.findViewById(101010);
            tv2 = (TextView) convertView.findViewById(101011);
        }
        if (tv1 != null )
          tv1.setText(getItem(position).text1 + " : " + Integer.toString(position));

        if (tv2 != null )
          tv2.setText(getItem(position).text2 );

        return convertView;

    }

    //-----------------------------------------------------
    @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        customView = null ;
        mListener.onGetCustomView(mDPFListView, convertView, position );
        if ( customView == null ){
          convertView = getListItem2View(position, convertView, parent);
        }
        else{
          convertView = customView ;
        }

        if (convertView == null){
           return null;
        }

        //if (position == mSelectedItem ) {
        if ( mSelectedItems.contains(position) ){
            convertView.setBackgroundColor( mSelectedColor );
        }
        else{
            convertView.setBackgroundColor(Color.TRANSPARENT);
        }

        return convertView;
    }

    //----------------------------------------------
    @Override
    public int getCount() {
        //return mIdMap.size();
        return mListener.onGetRowCount(mDPFListView);
    }

    /*
    //----------------------------------------------
    @Override
    public long getItemId(int position) {
      Station item = getItem(position);
      return mIdMap.get(item);
    }*/

    //------------------------------------------------
    @Override
    public boolean hasStableIds() {
      return true;
    }

  }

    //-----------------------------------------------------
    public void reLoad() {
        String[] values = new String[] {
        "D.P.F Android Native Components"
        };

        final ArrayList<Station> list = new ArrayList<Station>();

        for (int i = 0; i < values.length; ++i) {
             Station station = new Station();
             station.text1 = values[i] ;
             station.text2 = values[i] ;
           list.add(station);
        }

        final StableArrayAdapter mAdapter = new StableArrayAdapter(mContext,
        android.R.layout.simple_list_item_2, list);

        setAdapter(mAdapter);
        setCacheColorHint( Color.TRANSPARENT );
        setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);
        //setSelector(android.R.color.transparent);

        //----------------------------------------------------------------------
        // OnItemSelectedListener
        //----------------------------------------------------------------------
        setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
              public void onItemSelected(AdapterView parentView, View childView, int position, long id){
                mListener.onItemSelected(mDPFListView, childView, position, id);
         }

              public void onNothingSelected(AdapterView parentView) {
                mListener.onNothingSelected(mDPFListView);
              }
         });

        //----------------------------------------------------------------------
        // OnScrollListener
        // ScrollState:
        // SCROLL_STATE_IDLE = 0
        // SCROLL_STATE_TOUCH_SCROLL = 1
        // SCROLL_STATE_FLING = 2
        //----------------------------------------------------------------------
        setOnScrollListener(new OnScrollListener() {

            @Override
            public void onScrollStateChanged(AbsListView view, int scrollState) {

            }

            @Override
            public void onScroll(AbsListView view, int firstVisibleItem, int visibleItemCount, int totalItemCount) {
            }
        });

        //----------------------------------------------------------------------
        // OnItemClickListener
        //----------------------------------------------------------------------
        setOnItemClickListener(new OnItemClickListener() {

          @Override
          public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
             view.setSelected(true);
             view.getFocusables(position);

             if ( mListChoiceMode != ListView.CHOICE_MODE_MULTIPLE  ){
                 mSelectedItems.clear();
             }

             if ( mListChoiceMode != ListView.CHOICE_MODE_NONE && !mSelectedItems.contains(position) ){
                 mSelectedItems.add(position);
             }

             //mSelectedItem = position;

             mAdapter.notifyDataSetChanged();

             mListener.onItemClick(mDPFListView, view, position, id);
          }
        });

    }

    //-----------------------------------------------------
    public DPFListView(Context context)
    {
        super(context);
        mContext = context;
        mDPFListView = this;
    }

    //-----------------------------------------------------
    public void setListViewListener(DPFOnListViewListener onListViewlistener)
    {
       mListener = onListViewlistener;
    }

    //-----------------------------------------------------
    public void setCustomView(View cView)
    {
       customView = cView ;
    }

    //-----------------------------------------------------
    public void setItemSelectedColor(int color)
    {
       mSelectedColor = color ;
    }

    //-----------------------------------------------------
    public void clearSelectedItems()
    {
       mSelectedItems.clear();
    }

    //-----------------------------------------------------
    public void setListViewChoiceMode(int choiceMode)
    {
       mListChoiceMode = choiceMode ;
    }

}
