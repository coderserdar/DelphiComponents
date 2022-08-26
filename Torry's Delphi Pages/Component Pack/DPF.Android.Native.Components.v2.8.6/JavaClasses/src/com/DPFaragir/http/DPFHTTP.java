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
package com.DPFaragir.http;

import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;

import android.content.Context;
import android.content.pm.PackageManager;
import android.graphics.Canvas;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.widget.TextView;
import android.os.AsyncTask;
import android.os.PowerManager;
import android.os.Environment;
import android.util.Base64;
import android.util.Log;
import java.net.MalformedURLException;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.net.URL;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.HttpURLConnection;
import java.net.URLConnection;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import java.security.cert.X509Certificate;


public class DPFHTTP {

        private static final Integer OP_FINISHED       = 0;
        private static final Integer OP_CANCELED       = 1;
        private static final Integer OP_INTERNAL_ERROR = 2;
        private static final Integer OP_HTTPERROR      = 3;

        private HTTPTask mHTTPTask ;
        private Context mContext;
        private DPFOnHTTPListener mListener;
        private DPFHTTP mDPFHTTP;
        private Integer mHttpResponseCode=0;
        private String mHttpResponseMessage="";
        private String mError="";
        private String mFileName="";
        private String mURL="";
        private String mUserName = null;
        private String mPassword = null;
        private boolean mIsDownload ;
        private String[] mHeaders ;
        private String[] mFormFields ;

        //----------------------------------------------------------------------
        public void DPFHTTP()
        {
           mDPFHTTP = this;
        }

        //----------------------------------------------------------------------
        public void startDownload(Context context, String url, String fileName, String username, String password, DPFOnHTTPListener OnHTTPListener)
        {
            mContext = context ;
            mHttpResponseCode=0;
            mHttpResponseMessage="";
            mError="";
            mURL      = url;
            mFileName = fileName;
            mUserName = username;
            mPassword = password;

            mIsDownload = true ;

            mListener = OnHTTPListener;
            cancelTask();
            mHTTPTask = new HTTPTask(context);
            mHTTPTask.execute(url);
        }

        //----------------------------------------------------------------------
        public void startUpload(Context context, String fileName, String url, String[] httpHeaders, String[] formFields, DPFOnHTTPListener OnHTTPListener)
        {
            mContext = context ;
            mHttpResponseCode=0;
            mHttpResponseMessage="";
            mError="";
            mURL      = url;
            mFileName = fileName;
            //mUserName = username;
            //mPassword = password;

            mHeaders    = httpHeaders;
            mFormFields = formFields;

            mIsDownload = false ;

            mListener = OnHTTPListener;
            cancelTask();
            mHTTPTask = new HTTPTask(context);
            mHTTPTask.execute(url);
        }

        //----------------------------------------------------------------------
        public void cancelTask()
        {
             if(mHTTPTask !=null ){
                 if( mHTTPTask.getStatus() != AsyncTask.Status.FINISHED ){
                    mHTTPTask.cancel(true);
                 }
             }
        }

        //----------------------------------------------------------------------
        private class HTTPTask extends AsyncTask<String, Integer , Integer> {
            private Context context;

            //-----------------------------------------------------------------
            public HTTPTask(Context context) {
                this.context = context;
            }

            //-----------------------------------------------------------------
            @Override
            protected void onCancelled(Integer result){
               super.onCancelled(result);
               mListener.onCancelled(mDPFHTTP);
            }

            //-----------------------------------------------------------------
            @Override
            protected void onPreExecute(){
               super.onPreExecute();
               mListener.onStarted(mDPFHTTP);
            }

            //-----------------------------------------------------------------
            @Override
            protected void onProgressUpdate(Integer... progress) {
               super.onProgressUpdate(progress);
               mListener.onProgressUpdate(mDPFHTTP, progress[0], progress[1], progress[2]);
            }

            //-----------------------------------------------------------------
            @Override
            protected void onPostExecute(Integer result){
               super.onPostExecute(result);
               mListener.onFinished(mDPFHTTP, result, mHttpResponseCode, mHttpResponseMessage, mError);
            }

            //-----------------------------------------------------------------
            @Override
            protected Integer doInBackground(String... sUrl) {
               if (mIsDownload == true )
                   return downloadFile();
               else
                   return uploadFile();
            }


           //---------------------------------------------------------------------
           //  fix for
           //    Exception in thread "main" javax.net.ssl.SSLHandshakeException:
           //       sun.security.validator.ValidatorException:
           //           PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException:
           //               unable to find valid certification path to requested target
           //---------------------------------------------------------------------
            public int initSSL()
            {
                try
                {
                    TrustManager[] trustAllCerts = new TrustManager[] {
                       new X509TrustManager() {
                          public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                            return null;
                          }

                          public void checkClientTrusted(X509Certificate[] certs, String authType) {  }

                          public void checkServerTrusted(X509Certificate[] certs, String authType) {  }

                       }
                    };

                    SSLContext sc = SSLContext.getInstance("SSL");
                    sc.init(null, trustAllCerts, new java.security.SecureRandom());
                    HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());

                    // Create all-trusting host name verifier
                    HostnameVerifier allHostsValid = new HostnameVerifier() {
                        public boolean verify(String hostname, SSLSession session) {
                          return true;
                        }
                    };
                    // Install the all-trusting host verifier
                    HttpsURLConnection.setDefaultHostnameVerifier(allHostsValid);

                    return OP_FINISHED;
                } catch (Exception e)
                {
                    e.printStackTrace();
                    Log.e("SSL Initialization Error", "Exception : "+ e.getMessage(), e);

                    mError = "SSL Initialization Error : "+e.toString();
                    return OP_INTERNAL_ERROR;
                }
            }

            //------------------------------------------------------------------
            // @downloadFile
            //------------------------------------------------------------------
            public int downloadFile() {
                // take CPU lock to prevent CPU from going off if the user
                // presses the power button during download
                PowerManager.WakeLock wl=null;
                PowerManager pm=null;
                Boolean mWAKE_LOCK = (mContext.checkCallingOrSelfPermission("android.permission.WAKE_LOCK") == PackageManager.PERMISSION_GRANTED);
                if(mWAKE_LOCK){
                   pm = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
                   wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, getClass().getName());
                   wl.acquire();
                }

                try {
                    InputStream input = null;
                    OutputStream output = null;
                    HttpURLConnection connection = null;
                    URL url = null ;

                    try {

                        if (mUserName != null && mUserName.trim().length() > 0 && mPassword != null && mPassword.trim().length() > 0) {
                                Authenticator.setDefault(new Authenticator(){
                                    protected PasswordAuthentication getPasswordAuthentication() {
                                        return new PasswordAuthentication(mUserName, mPassword.toCharArray());
                                    }});
                        }

                        //URL url = new URL(mURL);
                        //connection = (HttpURLConnection) url.openConnection();
                        if((mURL.substring(0,8)).equalsIgnoreCase("https://")){
                            int retval = initSSL();
                            url = new URL(mURL);
                            connection = (HttpsURLConnection) url.openConnection();
                        }
                        else{
                           url = new URL(mURL);
                           connection = (HttpURLConnection) url.openConnection();
                        }

                        connection.connect();

                        /*
                        if (mUserName != null && mUserName.trim().length() > 0 && mPassword != null && mPassword.trim().length() > 0) {
                            final String authString = mUserName + ":" + mPassword;
                            connection.setRequestProperty("Authorization", "Basic " + Base64.encode(authString.getBytes(), Base64.DEFAULT));
                        }
                        */

                        // expect HTTP 200 OK, so we don't mistakenly save error report
                        // instead of the file
                        mHttpResponseCode = connection.getResponseCode() ;
                        if (mHttpResponseCode != HttpURLConnection.HTTP_OK)
                        {
                             mHttpResponseMessage = connection.getResponseMessage();
                             return OP_HTTPERROR;
                        }

                        // this will be useful to display download percentage
                        // might be -1: server did not report the length
                        int fileLength = connection.getContentLength();

                        // download the file
                        input = connection.getInputStream();

                        // String filepath = Environment.getExternalStorageDirectory().getPath();
                        // output = new FileOutputStream("/sdcard/file_name.extension");
                        output = new FileOutputStream( mFileName );

                        byte data[] = new byte[4096];
                        Integer _zero = 0;
                        Integer total = 0;
                        int count;
                        while ((count = input.read(data)) != -1) {
                            // allow canceling with back button
                            if (isCancelled())
                                return OP_CANCELED;
                            total += count;
                            // publishing the progress....
                            if (fileLength > 0) // only if total length is known
                                publishProgress((int) (total * 100 / fileLength), fileLength, total);
                            else
                                publishProgress(_zero, fileLength, total);

                            output.write(data, 0, count);
                        }
                    } catch (Exception e) {
                        mError = e.toString();
                        return OP_INTERNAL_ERROR;
                    }
                    finally
                    {
                        try
                        {
                            if (output != null)
                                output.close();
                            if (input != null)
                                input.close();
                        }
                        catch (IOException ignored)
                        {
                        }

                        if (connection != null)
                            connection.disconnect();
                    }
                }
                finally
                {
                   if(mWAKE_LOCK)
                     wl.release();
                }
                return OP_FINISHED;
            }

            //------------------------------------------------------------------
            // @uploadFile
            //------------------------------------------------------------------
            public int uploadFile( )
            {
                  HttpURLConnection conn = null;
                  URL url = null;
                  DataOutputStream dos = null;
                  String lineEnd = "\r\n";
                  String twoHyphens = "--";
                  String boundary = "*****";
                  int bytesRead, bytesAvailable, bufferSize;
                  byte[] buffer;
                  int maxBufferSize = 1024 * 1024 * 4;
                  File sourceFile = new File(mFileName);

                  if (!sourceFile.isFile()) {
                      mError = "Source File not exist :"+mFileName;
                      return OP_INTERNAL_ERROR;
                  }
                  else
                  {
                       try
                       {
                          if (mUserName != null && mUserName.trim().length() > 0 && mPassword != null && mPassword.trim().length() > 0)
                          {
                               Authenticator.setDefault(new Authenticator()
                               {
                                   protected PasswordAuthentication getPasswordAuthentication()
                                   {
                                       return new PasswordAuthentication(mUserName, mPassword.toCharArray());
                                   }
                               });
                          }

                          // open a URL connection to the Servlet
                          FileInputStream fileInputStream = new FileInputStream(sourceFile);

                         if((mURL.substring(0,8)).equalsIgnoreCase("https://")){
                             int retval = initSSL();
                             url = new URL(mURL);
                             conn = (HttpsURLConnection) url.openConnection();
                          }
                          else{
                            url = new URL(mURL);
                            conn = (HttpURLConnection) url.openConnection();
                          }
                          conn.setDoInput(true); // Allow Inputs
                          conn.setDoOutput(true); // Allow Outputs
                          conn.setUseCaches(false); // Don't use a Cached Copy
                          conn.setConnectTimeout(30000);
                          conn.setReadTimeout(30000);
                          conn.setRequestMethod("POST");
                          conn.setRequestProperty("Connection", "Keep-Alive");
                          conn.setRequestProperty("ENCTYPE", "multipart/form-data");
                          conn.setRequestProperty("Content-Type", "multipart/form-data;boundary=" + boundary);
                          //conn.setChunkedStreamingMode(maxBufferSize);
                          //conn.setRequestProperty("Transfer-Encoding","chunked");

                          for (int i = 0; i < mHeaders.length; i=i+2) {
                             conn.setRequestProperty(mHeaders[i], mHeaders[i+1]);
                          }

                          dos = new DataOutputStream(conn.getOutputStream());

                          for (int i = 0; i < mFormFields.length; i=i+2) {
                             dos.writeBytes(twoHyphens + boundary + lineEnd);
                             dos.writeBytes("Content-Disposition: form-data; name=\""+ mFormFields[i] +"\"" + lineEnd+ lineEnd);
                             dos.writeBytes("\""+mFormFields[i+1]+"\""+lineEnd);
                             dos.writeBytes(twoHyphens + boundary + twoHyphens + lineEnd);
                          }

                          dos.writeBytes(twoHyphens + boundary + lineEnd);
                          dos.writeBytes("Content-Disposition: form-data; name=\"inpFile\";filename=\""+ mFileName + "\"" + lineEnd);
                          dos.writeBytes(lineEnd);

                          // create a buffer of  maximum size
                          bytesAvailable = fileInputStream.available();
                          int fileLength = fileInputStream.available();
                          if(bytesAvailable == 0)
                          {
                          }

                          bufferSize = Math.min(bytesAvailable, maxBufferSize);
                          buffer = new byte[bufferSize];

                          // read file and write it into form...
                          bytesRead = fileInputStream.read(buffer, 0, bufferSize);


                          Integer total = 0;
                          while (bytesRead > 0)
                          {
                              total += bytesRead;
                              publishProgress((int) (total * 100 / fileLength), fileLength, total);

                              dos.write(buffer, 0, bufferSize);
                              bytesAvailable = fileInputStream.available();
                              bufferSize = Math.min(bytesAvailable, maxBufferSize);
                              bytesRead = fileInputStream.read(buffer, 0, bufferSize);
                          }

                          // send multipart form data necesssary after file data...
                          dos.writeBytes(lineEnd);
                          dos.writeBytes(twoHyphens + boundary + twoHyphens + lineEnd);

                          // Responses from the server (code and message)
                          String serverResponseMessage = conn.getResponseMessage();
                          mHttpResponseCode = conn.getResponseCode();

                          if(mHttpResponseCode == 200)
                          {

                          }
                          else
                          {
                          }

                          //close the streams //
                          fileInputStream.close();
                          dos.flush();
                          dos.close();
                          conn.disconnect();
                          return OP_FINISHED;

                       } catch (MalformedURLException ex)
                       {
                          ex.printStackTrace();
                          Log.e("Upload file to server", "error: " + ex.getMessage(), ex);

                          mError = "Upload file to server Exception :"+ex.toString();
                          return OP_INTERNAL_ERROR;

                       } catch (Exception e)
                       {
                           e.printStackTrace();
                           Log.e("Upload file to server Exception", "Exception : "+ e.getMessage(), e);

                           mError = "Upload file to server Exception :"+e.toString();
                           return OP_INTERNAL_ERROR;
                       }
                  } // End else block
            } // End Function
    }
}
