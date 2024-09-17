package com.cosmetic.gg.controller.me;

import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.common.validator.ModelValidator;
import com.cosmetic.gg.service.user.UserService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/me")
@AllArgsConstructor
@CrossOrigin
public class MeController {

	private final UserService userService;
	
	private final ModelValidator modelValidator;
	
	@PutMapping(value = "/change-password")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> changePassword(
			@RequestParam String newPassword,
		    @RequestParam String oldPassword,
		    @RequestParam String username,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			if (StringUtils.isNullOrEmpty(oldPassword) || StringUtils.isNullOrEmpty(newPassword))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.INVALID_DATA)),
		          HttpStatus.BAD_REQUEST
		        );
			
			Error error = userService.changePassword(oldPassword, newPassword, username);
		      if (!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, error),
		          HttpStatus.BAD_REQUEST
		        );
		      
	      return new ResponseEntity<>(
	    	        new Response<String>().success(transId, ErrorCode.SUCCESS.getEn()), HttpStatus.OK);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
