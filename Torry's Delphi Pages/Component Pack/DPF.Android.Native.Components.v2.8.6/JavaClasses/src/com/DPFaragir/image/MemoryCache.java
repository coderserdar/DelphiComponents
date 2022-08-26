package com.androidexample.lazyimagedownload;
 
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import android.graphics.Bitmap;
import android.util.Log;
 
public class MemoryCache {
 
    private static final String TAG = "MemoryCache";
     
    //Last argument true for LRU ordering
    private Map<String, Bitmap> cache = Collections.synchronizedMap(
            new LinkedHashMap<String, Bitmap>(10,1.5f,true));
     
   //current allocated size
    private long size=0; 
     
    //max memory cache folder used to download images in bytes
    private long limit=1000000; 
 
    public MemoryCache(){
         
        //use 25% of available heap size
        setLimit(Runtime.getRuntime().maxMemory()/4);
    }
     
    public void setLimit(long new_limit){
         
        limit=new_limit;
        Log.i(TAG, "MemoryCache will use up to "+limit/1024./1024.+"MB");
    }
 
    public Bitmap get(String id){
        try{
            if(!cache.containsKey(id))
                return null;
             
            return cache.get(id);
             
        }catch(NullPointerException ex){
            ex.printStackTrace();
            return null;
        }
    }
 
    public void put(String id, Bitmap bitmap){
        try{
            if(cache.containsKey(id))
                size-=getSizeInBytes(cache.get(id));
            cache.put(id, bitmap);
            size+=getSizeInBytes(bitmap);
            checkSize();
        }catch(Throwable th){
            th.printStackTrace();
        }
    }
     
    private void checkSize() {
        Log.i(TAG, "cache size="+size+" length="+cache.size());
        if(size>limit){
         
            //least recently accessed item will be the first one iterated
            Iterator<Entry<String, Bitmap>> iter=cache.entrySet().iterator();
               
            while(iter.hasNext()){
                Entry<String, Bitmap> entry=iter.next();
                size-=getSizeInBytes(entry.getValue());
                iter.remove();
                if(size<=limit)
                    break;
            }
            Log.i(TAG, "Clean cache. New size "+cache.size());
        }
    }
 
    public void clear() {
        try{
             // Clear cache
            cache.clear();
            size=0;
        }catch(NullPointerException ex){
            ex.printStackTrace();
        }
    }
 
    long getSizeInBytes(Bitmap bitmap) {
        if(bitmap==null)
            return 0;
        return bitmap.getRowBytes() * bitmap.getHeight();
    }
}