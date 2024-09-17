package com.cosmetic.gg.controller;

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
import com.cosmetic.gg.common.enums.EDiscountType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.request.discount.ProductDiscountRequest;
import com.cosmetic.gg.dto.request.discount.ProductItemDiscountRequest;
import com.cosmetic.gg.dto.request.discount.UserDiscountRequest;
import com.cosmetic.gg.dto.response.discount.DiscountResponse;
import com.cosmetic.gg.dto.response.product.ProductDetailResponse;
import com.cosmetic.gg.entity.discount.Discount;
import com.cosmetic.gg.entity.discount.ProductItemDiscount;
import com.cosmetic.gg.entity.discount.UserDiscount;
import com.cosmetic.gg.service.discount.DiscountService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/discount")
@AllArgsConstructor
@CrossOrigin
public class DiscountController {

	private final DiscountService discountService;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "keyword", required = false, defaultValue = "") String keyword,
			@RequestParam(value = "discountType", required = false, defaultValue = "") EDiscountType discountType,
			@RequestParam(value = "pageIndex", required = false, defaultValue = "1") int pageIndex,
			@RequestParam(value = "pageSize", required = false, defaultValue = "10") int pageSize) {
		String transId = UUID.randomUUID().toString();
		try {
			Map<String, Object> result = discountService.search(keyword, discountType, pageIndex, pageSize);
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
			@RequestBody @Valid Discount discount,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			discount.setId("");
			
			List<Error> errors = discountService.validator(discount);
			if(!errors.isEmpty())
				return new ResponseEntity<>(
					new Response<Error[]>().error(
							transId,
							errors.toArray(new Error[0])
					),
					HttpStatus.BAD_REQUEST
				);
			
			Discount discountEntity = discountService.create(discount);
			if (Objects.isNull(discountEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<Discount>().success(
						transId,
						discountEntity
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
			@RequestBody @Valid Discount discount,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			discount.setId(id);
			
			List<Error> errors = discountService.validator(discount);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			Discount discountEntity = discountService.update(discount);
			if (Objects.isNull(discountEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<Discount>().success(
							transId,
							discountEntity
							),
					HttpStatus.OK
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
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = discountService.delete(id);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
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
	
	@PutMapping(value = "/image/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> changeImage(
			@PathVariable(value = "id") String id,
			@RequestParam("image") MultipartFile imageFile,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = discountService.changeImage(imageFile, id);
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
	
	@PostMapping(value = "/user")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> addUser(
			@RequestBody @Valid UserDiscountRequest discountRequest,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			discountRequest.setId("");
			
			Error error = discountService.addUserDiscount(discountRequest);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
//	@DeleteMapping(value = "/user/{id}")
//	@ResponseStatus(value = HttpStatus.NO_CONTENT)
//	public ResponseEntity<Response<?>> deleteAllUserDiscount(
//			@PathVariable(value = "id") String id,
//			HttpServletRequest httpServletRequest) {
//		String transId = UUID.randomUUID().toString();
//		try {
//			Error error = discountService.deleteAllUserDiscount(id);
//			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
//				return new ResponseEntity<>(
//						new Response<Error>().error(transId, error),
//						HttpStatus.BAD_REQUEST
//				);
//			
//			return new ResponseEntity<>(
//					new Response<String>().success(
//							transId,
//							ErrorCode.SUCCESS.getEn()
//					),
//					HttpStatus.NO_CONTENT
//			);
//		}catch(Exception ex) {
//			log.error(CommonConstant.EXCEPTION, ex.getCause());
//			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
//		}
//	}
	
	@PostMapping(value = "/product")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> addProduct(
			@RequestBody @Valid ProductDiscountRequest productDiscountRequest,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			productDiscountRequest.setId("");
			
			Error error = discountService.addProductDiscount(productDiscountRequest);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "/product-item")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> addProductItem(
			@RequestBody @Valid ProductItemDiscountRequest productDiscountRequest,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			productDiscountRequest.setId("");
			
			Error error = discountService.addProductItemDiscount(productDiscountRequest);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
//	@DeleteMapping(value = "/product/{id}")
//	@ResponseStatus(value = HttpStatus.NO_CONTENT)
//	public ResponseEntity<Response<?>> deleteAllProductDiscount(
//			@PathVariable(value = "id") String id,
//			HttpServletRequest httpServletRequest) {
//		String transId = UUID.randomUUID().toString();
//		try {
//			Error error = discountService.deleteAllProductDiscount(id);
//			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
//				return new ResponseEntity<>(
//						new Response<Error>().error(transId, error),
//						HttpStatus.BAD_REQUEST
//				);
//			
//			return new ResponseEntity<>(
//					new Response<String>().success(
//							transId,
//							ErrorCode.SUCCESS.getEn()
//					),
//					HttpStatus.NO_CONTENT
//			);
//		}catch(Exception ex) {
//			log.error(CommonConstant.EXCEPTION, ex.getCause());
//			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
//		}
//	}

	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> detail(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			DiscountResponse response = discountService.detail(id);
			if (Objects.isNull(response))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<DiscountResponse>().success(
			          transId,
			          response
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
