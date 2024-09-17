package com.cosmetic.gg.authentication.controller;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.authentication.dto.LoginRequest;
import com.cosmetic.gg.authentication.dto.LoginResponse;
import com.cosmetic.gg.authentication.jwt.JwtUtil;
import com.cosmetic.gg.authentication.service.UserDetailsServiceImpl;
import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.email.EmailSender;
import com.cosmetic.gg.common.email.Message;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.validator.ModelValidator;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.model.UserModel;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.service.user.UserService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@AllArgsConstructor
@CrossOrigin
@RequestMapping(value = "api/v1/auth")
public class AuthencationController {

    @Autowired
    private JwtUtil jwtUtil;    
    
    @Autowired
	private EmailSender mailSender;
    
    @Autowired
    private PasswordEncoder bcryptEncoder;
    
    @Autowired
	private UserRepository userRepository;

    @Autowired
    private UserDetailsServiceImpl userDetailsServiceImpl;
    
    private final ModelValidator modelValidator;
    
    private final UserService userService;

    @PostMapping(
		value = "/register",
		produces = {MediaType.APPLICATION_JSON_VALUE},
	    consumes = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<Response<?>> register(
    		@RequestBody @Valid UserModel userModel,
    		BindingResult bindingResult,
    	    HttpServletRequest httpServletRequest) {
    	String transId = UUID.randomUUID().toString();
    	try {
    		userModel.setId("");
    		Response<Error> response = modelValidator.validateData(bindingResult, transId);
    	      if (response != null)
    	        return new ResponseEntity<>(
    	            response,
    	            HttpStatus.BAD_REQUEST
    	        );
    	      
    	      if (userModel.getUserRank() != EUserRank.MEMBER)
    	          return new ResponseEntity<>(
    	              new Response<Error>().error(
    	                  transId,
    	                  new Error().builder(
    	                      ErrorCode.INVALID_DATA,
    	                      "Cấp bậc người dùng không hợp lệ",
    	                      "User rank is invalid"
    	                  )),
    	              HttpStatus.BAD_REQUEST
    	          );
    	      
    	      List<Error> errors = userService.validator(userModel);
    	      if (!errors.isEmpty())
    	        return new ResponseEntity<>(
    	            new Response<Error[]>().error(
    	                transId,
    	                errors.toArray(new Error[0])
    	            ),
    	            HttpStatus.BAD_REQUEST
    	        );
    	      
    	      userModel = userService.register(userModel);
    	      if (Objects.isNull(userModel)) {
    	        ErrorCode errorCode = ErrorCode.FAILURE;
    	        return new ResponseEntity<>(
    	            new Response<Error>().error(transId, new Error().builder(errorCode)),
    	            HttpStatus.BAD_REQUEST
    	        );
    	      }
    	      
    	      return new ResponseEntity<>(
    	              new Response<UserModel>().success(
    	                  transId,
    	                  userModel
    	              ),
    	              HttpStatus.CREATED
    	          );
    	}catch(Exception ex) {
    		log.error(CommonConstant.EXCEPTION, ex.getCause());
    		throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
    	}
    }
    
    @PostMapping(
    		value = "/login",
    		produces = {MediaType.APPLICATION_JSON_VALUE},
    	    consumes = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity<?> login(
    		@RequestBody LoginRequest authenticationRequest,
    		BindingResult bindingResult,
    	    HttpServletRequest httpServletRequest){
    	String transId = UUID.randomUUID().toString();
    	try {
    		Response<Error> response = modelValidator.validateData(bindingResult, transId);
	  	      if (response != null)
	  	        return new ResponseEntity<>(
	  	            response,
	  	            HttpStatus.BAD_REQUEST
	  	        );
	  	      
	  	    User userEntity = userRepository.findByKey(authenticationRequest.getUsername());
	  	    if(userEntity == null) 
	  	    	return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
						HttpStatus.BAD_REQUEST
				);
	  	    if(userEntity.getStatus() != EStatus.ACTIVE)
	  	    	return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(ErrorCode.INVALID_STATUS)),
						HttpStatus.BAD_REQUEST
				);
	  	    Error error = userDetailsServiceImpl.authenticate(authenticationRequest.getUsername(), authenticationRequest.getPassword());
	  	    if (!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode())) {
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			}
	  	    
    		UserDetails userDetails = userDetailsServiceImpl.loadUserByUsername(authenticationRequest.getUsername());
    		String token = jwtUtil.generateToken(userDetails);
    		return ResponseEntity.ok(new LoginResponse(token));
    	}catch(Exception ex) {
    		log.error(CommonConstant.EXCEPTION, ex.getCause());
    		throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
    	}
    }
    
    @PostMapping(value = "/fotgot-password")
    @ResponseStatus(value = HttpStatus.OK)
    public ResponseEntity<Response<?>> forgotPassword(
    		@RequestParam String email,
    		HttpServletRequest httpServletRequest) {
    	String transId = UUID.randomUUID().toString();
    	try {
    		UserModel userModel = userService.getByKey(email);
    		if (Objects.isNull(userModel)) {
    	        ErrorCode errorCode = ErrorCode.INVALID_DATA;
    	        return new ResponseEntity<>(
    	          new Response<Error>().error(transId, new Error().builder(errorCode)),
    	          HttpStatus.BAD_REQUEST
    	        );
    	      }
    		if (userModel.getStatus() != EStatus.ACTIVE) {
    			ErrorCode errorCode = ErrorCode.INVALID_STATUS;
    			return new ResponseEntity<>(
    	    	          new Response<Error>().error(transId, new Error().builder(errorCode)),
    	    	          HttpStatus.BAD_REQUEST
    	    	        );
    		}
    		User userEntity = ModelMapper.map(userModel, User.class);
    		String password = StringUtils.generateRandomString();
    		userEntity.setPassword(bcryptEncoder.encode(password));
    		userEntity.prepareEntity();
    		UserModel result = ModelMapper.map(userRepository.save(userEntity), UserModel.class);
    		mailSender.sendMail(email, 
  	    		  Message.ACTIVE_ACCOUNT, 
  	    		  Message.DEAR_ACTIVE + userModel.getUsername() + ". " + Message.PASSWORD + password);
    		 return new ResponseEntity<>(
    			        new Response<String>().success(
    			          transId,
    			          ErrorCode.SUCCESS.getEn()
    			        ),
    			        HttpStatus.OK
    			      );
    	}catch(Exception ex) {
    		log.error(CommonConstant.EXCEPTION, ex.getCause());
    		throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
    	}
    }
    
    @PostMapping("/logout")
    public ResponseEntity<?> logout() {
      ResponseCookie cookie = jwtUtil.getCleanJwtCookie();
      return ResponseEntity.ok().header(HttpHeaders.SET_COOKIE, cookie.toString())
          .body("You've been logout!");
    }
}
