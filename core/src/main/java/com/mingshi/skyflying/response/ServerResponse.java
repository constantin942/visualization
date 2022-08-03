package com.mingshi.skyflying.response;

import com.mingshi.skyflying.exception.AiitExceptionCode;
import org.codehaus.jackson.map.annotate.JsonSerialize;

import java.io.Serializable;

@JsonSerialize(include=JsonSerialize.Inclusion.NON_NULL)
/**
 * 保证序列化json的时候,如果是null的对象,key也会消失
 */
public class ServerResponse<T> implements Serializable {

    private String code;
    private String msgCn;
    private String msgEn;
    private T data;

    public ServerResponse() {
    }

    private ServerResponse(String code) {
        this.code=code;
    }

    private ServerResponse(String code, T data) {
        this.code=code;
        this.data=data;
    }

    private ServerResponse(String code, String msgCn, String msgEn, T data) {
        this.code=code;
        this.msgCn=msgCn;
        this.msgEn=msgEn;
        this.data=data;
    }

    private ServerResponse(String code, String msgCn, String msgEn) {
        this.code=code;
        this.msgCn=msgCn;
        this.msgEn=msgEn;
    }

    public ServerResponse(AiitExceptionCode excCode) {
        this.code=excCode.getCode();
        this.msgCn=excCode.getMsgCn();
        this.msgEn=excCode.getMsgEn();
    }

    public ServerResponse(AiitExceptionCode excCode, T data) {
        this.code=excCode.getCode();
        this.msgCn=excCode.getMsgCn();
        this.msgEn=excCode.getMsgEn();
        this.data=data;
    }

    public T getData() {
        return data;
    }

    public String getMsgCn() {
        return msgCn;
    }

    public String getMsgEn() {
        return msgEn;
    }

    public void setMsgEn(String msgEn) {
        this.msgEn=msgEn;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code=code;
    }

    public void setMsgCn(String msgCn) {
        this.msgCn=msgCn;
    }

    public void setData(T data) {
        this.data=data;
    }

    public static <T> ServerResponse<T> createBySuccess() {
        return new ServerResponse<T>(AiitExceptionCode.SUCCESS.getCode());
    }

    public static <T> ServerResponse<T> createBySuccessMessage(String msgCn, String msgEn) {
        return new ServerResponse<T>(AiitExceptionCode.SUCCESS.getCode(), msgCn, msgEn);
    }

    public static <T> ServerResponse<T> createBySuccess(T data) {
        return new ServerResponse<T>(AiitExceptionCode.SUCCESS.getCode(), data);
    }
    public static <T> ServerResponse<T> createBySuccess(String msgCn, String msgEn,T data) {
        return new ServerResponse<T>(AiitExceptionCode.SUCCESS.getCode(), msgCn, msgEn,data);
    }
    public static <T> ServerResponse<T> createBySuccess(AiitExceptionCode excCode, T data) {
        ServerResponse<T> serverResponse=new ServerResponse<T>();
        serverResponse.setCode(excCode.getCode());
        serverResponse.setMsgEn(excCode.getMsgEn());
        serverResponse.setMsgCn(excCode.getMsgCn());
        serverResponse.setData(data);
        return serverResponse;
    }
    public static <T> ServerResponse<T> createBySuccess(AiitExceptionCode excCode) {
        ServerResponse<T> serverResponse=new ServerResponse<T>();
        serverResponse.setCode(excCode.getCode());
        serverResponse.setMsgEn(excCode.getMsgEn());
        serverResponse.setMsgCn(excCode.getMsgCn());
        return serverResponse;
    }

    public static <T> ServerResponse<T> createByError() {
        return new ServerResponse<T>(AiitExceptionCode.FAILURE.getCode(), (T) AiitExceptionCode.FAILURE.getMsgCn());
    }


  public static <T> ServerResponse<T> createByErrorMessage(String msgCn, String msgEn,T data) {
    return new ServerResponse<T>(AiitExceptionCode.FAILURE.getCode(), msgCn, msgEn,data);
  }

    public static <T> ServerResponse<T> createByErrorMessage(String errorMessage, String msgCn, String msgEn) {
        return new ServerResponse<T>(AiitExceptionCode.FAILURE.getCode(), msgCn, msgEn, (T) errorMessage);
    }

    public static <T> ServerResponse<T> createByErrorMessage(String msgCn, String msgEn) {
        return new ServerResponse<T>(AiitExceptionCode.FAILURE.getCode(), msgCn, msgEn);
    }

    public static <T> ServerResponse<T> createByErrorCodeMessage(String errorCode, String errorMessage) {
        return new ServerResponse<T>(errorCode, (T) errorMessage);
    }
}
