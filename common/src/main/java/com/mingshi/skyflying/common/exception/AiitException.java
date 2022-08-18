package com.mingshi.skyflying.common.exception;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/18
 */
public class AiitException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    /**
     * 错误码
     */
    protected String errorCode;
    /**
     * 错误信息
     */
    protected String errorMsg;

    public AiitException() {
        super();
    }

    public AiitException(AiitExceptionCode errorCode) {
        super(errorCode.getCode().toString());
        this.errorCode = errorCode.getCode().toString();
        this.errorMsg = errorCode.getMsgCn();
    }

    public AiitException(String errorMsg) {
        super(errorMsg);
        this.errorMsg = errorMsg;
    }

    public AiitException(String errorCode, String errorMsg) {
        super(errorCode);
        this.errorCode = errorCode;
        this.errorMsg = errorMsg;
    }

    public String getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(String errorCode) {
        this.errorCode = errorCode;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    public void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    @Override
    public Throwable fillInStackTrace() {
        return this;
    }
}