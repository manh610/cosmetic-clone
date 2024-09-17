package com.cosmetic.gg.controller;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.validator.ModelValidator;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.discount.Discount;
import com.cosmetic.gg.model.UserModel;
import com.cosmetic.gg.service.user.UserService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/user")
@AllArgsConstructor
@CrossOrigin
public class UserController {

	private final UserService userService;
	
	private final ModelValidator modelValidator;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "keyword", required = false, defaultValue = "") String keyword,
			@RequestParam(value = "status", required = false, defaultValue = "") EStatus status,
			@RequestParam(value = "roleId", required = false, defaultValue = "") String roleId,
			@RequestParam(value = "userRank", required = false, defaultValue = "") EUserRank userRank,
			@RequestParam(value = "pageIndex", required = false, defaultValue = "1") int pageIndex,
			@RequestParam(value = "pageSize", required = false, defaultValue = "10") int pageSize) {
		String transId = UUID.randomUUID().toString();
		try {
			Map<String, Object> result = userService.search(keyword, status, roleId, userRank, pageIndex, pageSize);
			Response<Object> response = new Response<>().success(
					transId,
					Integer.parseInt(result.get("totalItem").toString()),
					result.get("data")
			);
			return new ResponseEntity<>(
					response,
					HttpStatus.OK
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> create(
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
			
			List<Error> errors = userService.validator(userModel);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			userModel = userService.create(userModel);
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
	
	@PutMapping(value = "/{id}",
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> update(
			@PathVariable(value = "id") String id,
			@RequestBody @Valid UserModel userModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			userModel.setId(id);
			Response<Error> response = modelValidator.validateData(bindingResult, transId);
			if (response != null)
		        return new ResponseEntity<>(
		            response,
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
			
			userModel = userService.update(userModel);
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
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/image/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> changeImage(
			@PathVariable(value = "id") String id,
			@RequestParam("image") MultipartFile imageFile,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = userService.changeImage(imageFile, id);
			if (errorCode != ErrorCode.SUCCESS) {
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(errorCode)),
		          HttpStatus.BAD_REQUEST
		        );
		      }
			return new ResponseEntity<>(
	            new Response<String>().success(
	              transId,
	              errorCode.getEn()
	            ),
	            HttpStatus.NO_CONTENT
	          );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@DeleteMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> delete(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			Error error = userService.delete(id);
			if (!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode())) {
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.NO_CONTENT
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> detail(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			UserModel userModel = userService.detail(id);
			if (Objects.isNull(userModel))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<UserModel>().success(
			          transId,
			          userModel
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/block/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> block(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = userService.blockAccount(id);
			if (!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode())) {
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			}
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.NO_CONTENT
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/recover/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> recover(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = userService.recoverAccount(id);
			if (!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode())) {
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			}
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.NO_CONTENT
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/user-infor")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getUser(
			@RequestParam String accessToken,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			User userEntity = userService.getUserFromToken(accessToken);
			return new ResponseEntity<>(
					new Response<User>().success(
							transId,
							userEntity
					),
					HttpStatus.OK
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/expireTime")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getExpireTime(
			@RequestParam String accessToken,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			LocalDateTime expireTime = userService.getExpireTimeToken(accessToken);
			return new ResponseEntity<>(
					new Response<LocalDateTime>().success(
							transId,
							expireTime
					),
					HttpStatus.OK
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/discount/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getDiscountByUser(
			@PathVariable(value = "id") String id,
			@RequestParam(value = "use", required = false, defaultValue = "") Boolean use,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			List<Discount> discounts = userService.getDiscountByUser(id, use);
			if (Objects.isNull(discounts))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<List<Discount>>().success(
			          transId,
			          discounts.size(),
			          discounts
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
