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
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.attribute.ValueDetailResponse;
import com.cosmetic.gg.dto.response.product.ProductDetailResponse;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.product.ProductModel;
import com.cosmetic.gg.service.product.ProductService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/product")
@AllArgsConstructor
@CrossOrigin
public class ProductController {

	private final ProductService productService;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "keyword", required = false, defaultValue = "") String keyword,
			@RequestParam(value = "status", required = false, defaultValue = "") EStatus status,
			@RequestParam(value = "brandId", required = false, defaultValue = "") String brandId,
			@RequestParam(value = "categoryId", required = false, defaultValue = "") String categoryId,
			@RequestParam(value = "skinTypeId", required = false, defaultValue = "") String skinTypeId,
			@RequestParam(value = "min", required = false, defaultValue = "-1.0f") float min,
			@RequestParam(value = "max", required = false, defaultValue = "-1.0f") float max,
			@RequestParam(value = "isDate", required = false, defaultValue = "") Boolean isDate,
			@RequestParam(value = "pageIndex", required = false, defaultValue = "1") int pageIndex,
			@RequestParam(value = "pageSize", required = false, defaultValue = "10") int pageSize) {
		String transId = UUID.randomUUID().toString();
		try {
			Map<String, Object> result = productService.search(keyword, status, brandId, categoryId, skinTypeId, min, max, 
					isDate, pageIndex, pageSize);
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
			@RequestBody @Valid ProductModel productModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			productModel.setId("");
			
			List<Error> errors = productService.validator(productModel);
			if(!errors.isEmpty())
				return new ResponseEntity<>(
					new Response<Error[]>().error(
							transId,
							errors.toArray(new Error[0])
					),
					HttpStatus.BAD_REQUEST
				);
			
			Product productEntity = productService.create(productModel);
			if (Objects.isNull(productEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<Product>().success(
						transId,
						productEntity
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
			@RequestBody @Valid ProductModel productModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			productModel.setId(id);
			
			List<Error> errors = productService.validator(productModel);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			Product productEntity = productService.update(productModel);
			if (Objects.isNull(productEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<Product>().success(
							transId,
							productEntity
							),
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/photo/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> changePhoto(
			@PathVariable(value = "id") String id,
			@RequestParam("photo") MultipartFile photo,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = productService.changePhoto(photo, id);
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
	
	@DeleteMapping(value = "/image/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> deleteImage(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = productService.deleteImage(id);
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
	
	@DeleteMapping(value = "/images")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> deleteMultiImage(
			@RequestBody List<String> ids,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = productService.deleteMultiImage(ids);
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
	
	@PutMapping(value = "/images/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> changImages(
			@PathVariable(value = "id") String id,
			@RequestParam("images") List<MultipartFile> images,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = productService.changeImage(images, id);
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
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = productService.delete(id);
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
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> detail(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			ProductDetailResponse response = productService.detail(id);
			if (Objects.isNull(response))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<ProductDetailResponse>().success(
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
	
	@GetMapping(value = "/value/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getValueDetail(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			ValueDetailResponse response = productService.getValueByProductItem(id);
			if (Objects.isNull(response))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<ValueDetailResponse>().success(
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
