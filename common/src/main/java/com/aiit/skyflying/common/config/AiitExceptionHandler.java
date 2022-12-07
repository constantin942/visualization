package com.aiit.skyflying.common.config;

import com.aiit.skyflying.common.exception.AiitException;
import com.aiit.skyflying.common.response.ServerResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

@ControllerAdvice
@Slf4j
public class AiitExceptionHandler {

    @ExceptionHandler(AiitException.class)
    @ResponseBody
    public ServerResponse myExceptionHandle(AiitException e) {

        log.error("发生业务异常！原因是：{}", e.getErrorMsg());

        return ServerResponse.createByErrorCodeMessage("500", e.getErrorMsg());
    }

    /**
     * 处理空指针的异常
     *
     * @param e
     * @return
     */
    @ExceptionHandler(value = NullPointerException.class)
    @ResponseBody
    public ServerResponse nullExceptionHandler(NullPointerException e) {
        log.error("发生空指针异常！原因是: ", e);
        return ServerResponse.createByErrorCodeMessage("500", e.getMessage());
    }

    /**
     * 处理其他异常
     *
     * @param e
     * @return
     */
    @ExceptionHandler(value = Exception.class)
    @ResponseBody
    public ServerResponse exceptionHandler(Exception e) {
        log.error("未知异常！原因是: ", e);
        return ServerResponse.createByErrorCodeMessage("500", e.getMessage());
    }
}
