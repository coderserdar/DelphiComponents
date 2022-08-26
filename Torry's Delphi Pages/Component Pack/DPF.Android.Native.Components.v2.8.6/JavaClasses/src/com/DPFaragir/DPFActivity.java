// ------------------------------------------------------------------------------
// com.DPFaragir.DPFActivity Java Sub Class
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

import android.app.Activity;
import android.app.NativeActivity;
import android.content.res.Resources;
import android.os.Bundle;
//import android.widget.TextView;
//import android.widget.EditText;
import android.content.Context;
import android.webkit.WebView;
import android.widget.Toast;
import android.widget.LinearLayout;

public class DPFActivity extends Activity{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        LinearLayout layout = new LinearLayout(this);
        setContentView(layout);
        layout.setOrientation(LinearLayout.VERTICAL);

        //TextView text = new EditText(this);
        //text.setText("Hello World, Android - wwww.dpfaragir.com");
        //layout.addView(text);
    }
}
